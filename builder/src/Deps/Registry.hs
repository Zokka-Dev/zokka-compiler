{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings, TupleSections #-}
module Deps.Registry
  ( Registry(..)
  , KnownVersions(..)
  , ZokkaRegistries(..)
  , RegistryKey(..)
  , read
  , fetch
  , update
  , latest
  , getVersions
  , getVersions'
  , mergeRegistries
  , lookupPackageRegistryKey
  , createAuthHeader
  )
  where


import Prelude hiding (read)
import Control.Monad (liftM2, join)
import Data.Binary (Binary, get, put)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Deps.Website as Website
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Parse.Primitives as P
import qualified Reporting.Exit as Exit
import qualified Stuff
import Elm.CustomRepositoryData (CustomSingleRepositoryData(..), CustomRepositoriesData(..), RepositoryUrl, RepositoryType(..), SinglePackageLocationData(..), PackageUrl, DefaultPackageServerRepo (..), PZRPackageServerRepo(..), RepositoryAuthToken)
import Data.Binary.Get (Get)
import Data.Word (Word8)
import qualified Data.Set as Set
import qualified Data.NonEmptyList as NE
import Data.Vector.Internal.Check (HasCallStack)
import Data.Map.Utils (exchangeKeys, invertMap)
import File (Time, getTime)
import Stuff (ZokkaCustomRepositoryConfigFilePath(unZokkaCustomRepositoryConfigFilePath))
import Http (Header)
import qualified Data.Utf8 as Utf8
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString



-- REGISTRY

-- We need to differentiate among registries created from different repositories
-- because of how we perform updates of the registry. We need to know how many
-- packages came from a given repository to pass to the /all-packages/since
-- endpoint. So we can't start from the outset with all the registries merged.
data ZokkaRegistries = ZokkaRegistries
  -- This last modification field is used for caching purposes so that we can speed
  -- things along if our custom repository config hasn't been changed (we just need
  -- to update pre-existing repos). If the config has been changed then we need to
  -- re-fetch from scratch.
  { _lastModificationTimeOfCustomRepoConfig :: Time
  , _registries :: !(Map.Map RegistryKey Registry)
  , _packagesToLocations :: !(Map.Map Pkg.Name (Map.Map V.Version RegistryKey))
  }
  deriving (Show, Eq)

-- MAP HELPERS

knownVersionsToNEListOfVersions :: KnownVersions -> NE.List V.Version
knownVersionsToNEListOfVersions (KnownVersions newest rest) = NE.List newest rest


zokkaRegistriesFromRegistriesMap :: Time -> Map.Map RegistryKey Registry -> ZokkaRegistries
zokkaRegistriesFromRegistriesMap custmoRepoConfigLastModificationTime registriesMap =
  let
    --FIXME: Deal with what happens when we have multiple registries with the same
    -- version of a package. Right now we just essentially randomly choose one
    -- (subject to the ordering of maps)
    registryKeysToPkgVersions = Map.map _versions registriesMap
    pkgNamesToRegistryAndKnownVersions = exchangeKeys registryKeysToPkgVersions
    pkgNamesToRegistryAndAllVersions = (fmap . fmap) knownVersionsToNEListOfVersions pkgNamesToRegistryAndKnownVersions
    pkgNamesToAllVersionsAndRegistry = fmap invertMap pkgNamesToRegistryAndAllVersions
    pkgNamesToSingleVersionAndRegistry = (fmap . fmap) NE.head pkgNamesToAllVersionsAndRegistry
  in
    ZokkaRegistries{_lastModificationTimeOfCustomRepoConfig=custmoRepoConfigLastModificationTime, _registries=registriesMap, _packagesToLocations=pkgNamesToSingleVersionAndRegistry}


lookupPackageRegistryKey :: ZokkaRegistries -> Pkg.Name -> V.Version -> Maybe RegistryKey
lookupPackageRegistryKey ZokkaRegistries{_packagesToLocations=packagesToLocations} pkgName pkgVersion =
  do
    versions <- Map.lookup pkgName packagesToLocations
    Map.lookup pkgVersion versions


data RegistryKey
  = RepositoryUrlKey CustomSingleRepositoryData
  | PackageUrlKey SinglePackageLocationData
  deriving (Eq, Ord, Show)


combineKnownVersions :: KnownVersions -> KnownVersions -> KnownVersions
combineKnownVersions (KnownVersions newest0 previous0) (KnownVersions newest1 previous1) =
  KnownVersions (max newest0 newest1) (min newest0 newest1 : (previous0 ++ previous1))


combineRegistry :: Registry -> Registry -> Registry
combineRegistry (Registry count0 versions0) (Registry count1 versions1) =
  Registry (count0 + count1) (Map.unionWith combineKnownVersions versions0 versions1)


emptyRegistry :: Registry
emptyRegistry = Registry 0 Map.empty


mergeRegistries :: ZokkaRegistries -> Registry
mergeRegistries ZokkaRegistries{_registries=registries} = Map.foldl combineRegistry emptyRegistry registries


data Registry =
  Registry
    { _count :: !Int
    , _versions :: !(Map.Map Pkg.Name KnownVersions)
    }
    deriving (Eq, Show)


data KnownVersions =
  KnownVersions
    { _newest :: V.Version
    , _previous :: ![V.Version]
    }
    deriving (Eq, Show)



-- READ


read :: HasCallStack => Stuff.ZokkaSpecificCache -> IO (Maybe ZokkaRegistries)
read cache =
  File.readBinary (Stuff.registry cache)



-- FETCH



fetch :: Http.Manager -> Stuff.ZokkaSpecificCache -> CustomRepositoriesData -> Time -> IO (Either Exit.RegistryProblem ZokkaRegistries)
fetch manager cache (CustomRepositoriesData customFullRepositories singlePackageLocations) customRepoConfigLastModified =
  do
    -- FIXME: this is pretty awful
    let fetchSingleRepo repoData = (,) (RepositoryUrlKey repoData) <$> fetchSingleCustomRepository manager repoData
    fullRepositoryFetchResults <- traverse fetchSingleRepo customFullRepositories
    let singlePackageRegistries = (\locData -> (PackageUrlKey locData, createRegistryFromSinglePackageLocation locData)) <$> singlePackageLocations
    let allResults = (++) singlePackageRegistries <$> mapM sequence fullRepositoryFetchResults
    case allResults of
      Left err -> pure $ Left err
      Right registries -> do
        let path = Stuff.registry cache
        let registry = Map.fromList registries
        File.writeBinary path (zokkaRegistriesFromRegistriesMap customRepoConfigLastModified registry)
        pure $ Right (zokkaRegistriesFromRegistriesMap customRepoConfigLastModified registry)


createRegistryFromSinglePackageLocation :: SinglePackageLocationData -> Registry
createRegistryFromSinglePackageLocation SinglePackageLocationData{_packageName=packageName, _version=version} =
    Registry {_count = 0, _versions = Map.singleton packageName KnownVersions {_newest = version, _previous=[]}}


fetchSingleCustomRepository :: Http.Manager -> CustomSingleRepositoryData -> IO (Either Exit.RegistryProblem Registry)
fetchSingleCustomRepository manager customRepositoryData =
  case customRepositoryData of
    DefaultPackageServerRepoData defaultPackageServerRepo -> 
      let
        repositoryUrl = _defaultPackageServerRepoTypeUrl defaultPackageServerRepo
      in
      fetchFromRepositoryUrl manager repositoryUrl
    PZRPackageServerRepoData pzrPackageServerRepo ->
      let
        repositoryUrl = _pzrPackageServerRepoTypeUrl pzrPackageServerRepo
        repositoryAuthToken = _pzrPackageServerRepoAuthToken pzrPackageServerRepo
      in
      undefined


fetchFromRepositoryUrl :: Http.Manager -> RepositoryUrl -> IO (Either Exit.RegistryProblem Registry)
fetchFromRepositoryUrl manager repositoryUrl =
  post manager repositoryUrl "/all-packages" allPkgsDecoder $
    \versions ->
      do  let size = Map.foldr' addEntry 0 versions
          pure $ Registry size versions


addEntry :: KnownVersions -> Int -> Int
addEntry (KnownVersions _ vs) count =
  count + 1 + length vs


allPkgsDecoder :: D.Decoder () (Map.Map Pkg.Name KnownVersions)
allPkgsDecoder =
  let
    keyDecoder =
      Pkg.keyDecoder bail

    versionsDecoder =
      D.list (D.mapError (\_ -> ()) V.decoder)

    toKnownVersions versions =
      case List.sortBy (flip compare) versions of
        v:vs -> return (KnownVersions v vs)
        []   -> D.failure ()
  in
  D.dict keyDecoder (toKnownVersions =<< versionsDecoder)



-- UPDATE

update :: Http.Manager -> Stuff.ZokkaSpecificCache -> ZokkaRegistries -> Time -> IO (Either Exit.RegistryProblem ZokkaRegistries)
update manager cache zokkaRegistries customRepoConfigLastModified =
  do
    let registriesMap = _registries zokkaRegistries
    let listOfProblemsOrKeyRegistryPairs = traverse (\(k, v) -> fmap (fmap ((,) k)) (updateSingleRegistry manager k v)) (Map.toList registriesMap)
    newRegistryOrError <- sequence <$> listOfProblemsOrKeyRegistryPairs
    let newRegistryOrError' = fmap Map.fromList newRegistryOrError
    case newRegistryOrError' of
      Left err -> pure $ Left err
      Right newRegistry ->
        do
          -- FIXME: There's gotta be a faster way of doing this
          let newZokkaRegistries = zokkaRegistriesFromRegistriesMap customRepoConfigLastModified newRegistry
          _ <- File.writeBinary (Stuff.registry cache) newZokkaRegistries
          pure $ Right newZokkaRegistries


updateSingleRegistry :: Http.Manager -> RegistryKey -> Registry -> IO (Either Exit.RegistryProblem Registry)
updateSingleRegistry manager registryKey registry =
  case registryKey of
    -- FIXME: need to deal with bare repos
    RepositoryUrlKey repositoryData -> case repositoryData of
      DefaultPackageServerRepoData defaultPackageServerRepo -> 
        updateSingleRegistryFromStandardElmRepo manager (_defaultPackageServerRepoTypeUrl defaultPackageServerRepo) registry
      -- FIXME: This is inefficient, in that we just download the entire custom repo every time, but we hope that custom repos are still quite small
      PZRPackageServerRepoData pzrPackageServerRepo -> updateSingleRegistryFromPZRRepo manager pzrPackageServerRepo registry
    -- With package URLs, only one package can correspond to a single URL so there is no sensible notion of "update"
    PackageUrlKey _ -> pure $ Right registry


updateSingleRegistryFromStandardElmRepo :: Http.Manager -> RepositoryUrl -> Registry -> IO (Either Exit.RegistryProblem Registry)
updateSingleRegistryFromStandardElmRepo manager repositoryUrl oldRegistry@(Registry size packages) =
  post manager repositoryUrl ("/all-packages/since/" ++ show size) (D.list newPkgDecoder) $
    \news ->
      case news of
        [] ->
          pure oldRegistry

        _:_ ->
          let
            newSize = size + length news
            newPkgs = foldr addNew packages news
            newRegistry = Registry newSize newPkgs
          in
            pure newRegistry

authSchemeName :: ByteString
authSchemeName = "CustomZokkaRepoAuthToken"

createAuthHeader :: RepositoryAuthToken -> Header
createAuthHeader authTokenValue = ("Authorization", ByteString.concat [authSchemeName, " ", authTokenValueAsChars])
  where
    authTokenValueAsChars = pack (Utf8.toChars authTokenValue)


updateSingleRegistryFromPZRRepo :: Http.Manager -> PZRPackageServerRepo -> Registry -> IO (Either Exit.RegistryProblem Registry)
updateSingleRegistryFromPZRRepo manager pzrPackageServerRepo oldRegistry@(Registry size packages) =
  let
    serverUrl = _pzrPackageServerRepoTypeUrl pzrPackageServerRepo
    repoAuthToken = _pzrPackageServerRepoAuthToken pzrPackageServerRepo
  in
  postWithHeaders manager serverUrl ("/all-packages/since/" ++ show size) [createAuthHeader repoAuthToken] (D.list newPkgDecoder) $
    \news ->
      case news of
        [] ->
          pure oldRegistry

        _:_ ->
          let
            newSize = size + length news
            newPkgs = foldr addNew packages news
            newRegistry = Registry newSize newPkgs
          in
            pure newRegistry


addNew :: (Pkg.Name, V.Version) -> Map.Map Pkg.Name KnownVersions -> Map.Map Pkg.Name KnownVersions
addNew (name, version) versions =
  let
    add maybeKnowns =
      case maybeKnowns of
        Just (KnownVersions v vs) ->
          KnownVersions version (v:vs)

        Nothing ->
          KnownVersions version []
  in
  Map.alter (Just . add) name versions



-- NEW PACKAGE DECODER


newPkgDecoder :: D.Decoder () (Pkg.Name, V.Version)
newPkgDecoder =
  D.customString newPkgParser bail


newPkgParser :: P.Parser () (Pkg.Name, V.Version)
newPkgParser =
  do  pkg <- P.specialize (\_ _ _ -> ()) Pkg.parser
      P.word1 0x40 {-@-} bail
      vsn <- P.specialize (\_ _ _ -> ()) V.parser
      return (pkg, vsn)


bail :: row -> col -> ()
bail _ _ =
  ()



-- LATEST

customSingleRepositoryDataToRegistryKey :: CustomSingleRepositoryData -> RegistryKey
customSingleRepositoryDataToRegistryKey = RepositoryUrlKey

singlePackageLocationDataToRegistryKey :: SinglePackageLocationData -> RegistryKey
singlePackageLocationDataToRegistryKey = PackageUrlKey

doesRegistryAgreeWithCustomRepositoriesData :: CustomRepositoriesData -> ZokkaRegistries -> Bool
doesRegistryAgreeWithCustomRepositoriesData (CustomRepositoriesData fullRepositories singlePackages) registry =
  Set.fromList allRegistryKeys == Map.keysSet (_registries registry)
    where
      allRegistryKeys = (customSingleRepositoryDataToRegistryKey <$> fullRepositories) ++ (singlePackageLocationDataToRegistryKey <$> singlePackages)

latest :: Http.Manager -> CustomRepositoriesData -> Stuff.ZokkaSpecificCache -> Stuff.ZokkaCustomRepositoryConfigFilePath -> IO (Either Exit.RegistryProblem ZokkaRegistries)
latest manager customRepositoriesData cache customRepoConfigFilePath =
  do
    maybeOldRegistry <- read cache
    customRepoConfigFilePathLastModified <- getTime (unZokkaCustomRepositoryConfigFilePath customRepoConfigFilePath)
    case maybeOldRegistry of
      Just oldRegistry -> if doesRegistryAgreeWithCustomRepositoriesData customRepositoriesData oldRegistry
        then update manager cache oldRegistry customRepoConfigFilePathLastModified
        -- FIXME: This is too conservative
        else fetch manager cache customRepositoriesData customRepoConfigFilePathLastModified
      Nothing -> fetch manager cache customRepositoriesData customRepoConfigFilePathLastModified



-- GET VERSIONS

compareVersionToKnownVersions :: V.Version -> Maybe KnownVersions -> KnownVersions
compareVersionToKnownVersions version knownVersionsMaybe =
  case knownVersionsMaybe of
    Just (KnownVersions newest others) -> KnownVersions (max newest version) (min newest version : others)
    Nothing -> KnownVersions version []


versionsToKnownVersions :: [V.Version] -> Maybe KnownVersions
versionsToKnownVersions = foldr (\v acc -> Just $ compareVersionToKnownVersions v acc) Nothing


getVersions :: Pkg.Name -> ZokkaRegistries -> Maybe KnownVersions
getVersions name ZokkaRegistries{_packagesToLocations=packagesToLocations} =
  do
    versionsMap <- Map.lookup name packagesToLocations
    let versions = Map.keys versionsMap
    versionsToKnownVersions versions

getVersions' :: Pkg.Name -> ZokkaRegistries -> Either [Pkg.Name] KnownVersions
getVersions' name zokkaRegistry =
  case getVersions name zokkaRegistry of
    Just kvs -> Right kvs
    -- FIXME: Maybe a faster way than just brute-force merging?
    Nothing -> Left $ Pkg.nearbyNames name (Map.keys (_versions $ mergeRegistries zokkaRegistry))



-- POST

postWithHeaders :: Http.Manager -> RepositoryUrl -> String -> [Header] -> D.Decoder x a -> (a -> IO b) -> IO (Either Exit.RegistryProblem b)
postWithHeaders manager repositoryUrl path headers decoder callback =
  let
    url = Website.route repositoryUrl path []
  in
  Http.post manager url headers Exit.RP_Http $
    \body ->
      case D.fromByteString decoder body of
        Right a -> Right <$> callback a
        Left _ -> return $ Left $ Exit.RP_Data url body

post :: Http.Manager -> RepositoryUrl -> String -> D.Decoder x a -> (a -> IO b) -> IO (Either Exit.RegistryProblem b)
post manager repositoryUrl path decoder callback =
  postWithHeaders manager repositoryUrl path [] decoder callback



-- BINARY

instance Binary RegistryKey where
  get = do
    t <- get :: Get Word8
    case t of
      0 -> do
        customSingleRepositoryData <- get :: Get CustomSingleRepositoryData
        pure $ RepositoryUrlKey customSingleRepositoryData
      1 -> do
        singlePackageLocationData <- get :: Get SinglePackageLocationData
        pure $ PackageUrlKey singlePackageLocationData
      _ ->
        -- FIXME: Better error message
        error "Corrupt registry key! We should only have a 0 or 1 here."

  put registryKey = case registryKey of
    RepositoryUrlKey repositoryUrl -> do
      put (0 :: Word8)
      put repositoryUrl
    PackageUrlKey packageUrl -> do
      put (1 :: Word8)
      put packageUrl


instance Binary Registry where
  get = liftM2 Registry get get
  put (Registry a b) = put a >> put b


instance Binary KnownVersions where
  get = liftM2 KnownVersions get get
  put (KnownVersions a b) = put a >> put b


instance Binary ZokkaRegistries where
  get = do
    configFileTime <- get :: Get Time
    registries <- get :: Get (Map.Map RegistryKey Registry)
    packagesToLocations <- get :: Get (Map.Map Pkg.Name (Map.Map V.Version RegistryKey))
    pure $ ZokkaRegistries{_lastModificationTimeOfCustomRepoConfig=configFileTime, _registries=registries, _packagesToLocations=packagesToLocations}

  put (ZokkaRegistries configFileTime registries packagesToLocations) = do
    put configFileTime
    put registries
    put packagesToLocations