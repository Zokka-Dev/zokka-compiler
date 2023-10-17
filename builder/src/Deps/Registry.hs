{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings, TupleSections #-}
module Deps.Registry
  ( Registry(..)
  , KnownVersions(..)
  , ZelmRegistries(..)
  , RegistryKey(..)
  , read
  , fetch
  , update
  , latest
  , getVersions
  , getVersions'
  , mergeRegistries
  , lookupPackageRegistryKey
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
import Elm.CustomRepositoryData (CustomSingleRepositoryData(..), CustomRepositoriesData(..), RepositoryUrl, RepositoryType(..), SinglePackageLocationData(..), PackageUrl)
import Data.Binary.Get (Get)
import Data.Word (Word8)
import qualified Data.Set as Set
import qualified Data.NonEmptyList as NE
import Data.Vector.Internal.Check (HasCallStack)
import Data.Map.Utils (exchangeKeys, invertMap)



-- REGISTRY

-- We need to differentiate among registries created from different repositories
-- because of how we perform updates of the registry. We need to know how many
-- packages came from a given repository to pass to the /all-packages/since
-- endpoint. So we can't start from the outset with all the registries merged.
data ZelmRegistries = ZelmRegistries
  { _registries :: !(Map.Map RegistryKey Registry)
  , _packagesToLocations :: !(Map.Map Pkg.Name (Map.Map V.Version RegistryKey))
  }
  deriving (Show, Eq)

-- MAP HELPERS

knownVersionsToNEListOfVersions :: KnownVersions -> NE.List V.Version
knownVersionsToNEListOfVersions (KnownVersions newest rest) = NE.List newest rest


zelmRegistriesFromRegistriesMap :: Map.Map RegistryKey Registry -> ZelmRegistries
zelmRegistriesFromRegistriesMap registriesMap =
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
    ZelmRegistries{_registries=registriesMap, _packagesToLocations=pkgNamesToSingleVersionAndRegistry}


lookupPackageRegistryKey :: ZelmRegistries -> Pkg.Name -> V.Version -> Maybe RegistryKey
lookupPackageRegistryKey ZelmRegistries{_packagesToLocations=packagesToLocations} pkgName pkgVersion =
  do
    versions <- Map.lookup pkgName packagesToLocations
    Map.lookup pkgVersion versions


data RegistryKey
  = RepositoryUrlKey RepositoryUrl
  | PackageUrlKey PackageUrl
  deriving (Eq, Ord, Show)


combineKnownVersions :: KnownVersions -> KnownVersions -> KnownVersions
combineKnownVersions (KnownVersions newest0 previous0) (KnownVersions newest1 previous1) =
  KnownVersions (max newest0 newest1) (min newest0 newest1 : (previous0 ++ previous1))


combineRegistry :: Registry -> Registry -> Registry
combineRegistry (Registry count0 versions0) (Registry count1 versions1) =
  Registry (count0 + count1) (Map.unionWith combineKnownVersions versions0 versions1)


emptyRegistry :: Registry
emptyRegistry = Registry 0 Map.empty


mergeRegistries :: ZelmRegistries -> Registry
mergeRegistries ZelmRegistries{_registries=registries} = Map.foldl combineRegistry emptyRegistry registries


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


read :: HasCallStack => Stuff.ZelmSpecificCache -> IO (Maybe ZelmRegistries)
read cache =
  File.readBinary (Stuff.registry cache)



-- FETCH



fetch :: Http.Manager -> Stuff.ZelmSpecificCache -> CustomRepositoriesData -> IO (Either Exit.RegistryProblem ZelmRegistries)
fetch manager cache (CustomRepositoriesData customFullRepositories singlePackageLocations) =
  do
    -- FIXME: this is pretty awful
    let fetchSingleRepo repoData = (,) (RepositoryUrlKey $ _repositoryUrl repoData) <$> fetchSingleCustomRepository manager repoData
    fullRepositoryFetchResults <- traverse fetchSingleRepo customFullRepositories
    let singlePackageRegistries = (\locData -> (PackageUrlKey $ _url locData, createRegistryFromSinglePackageLocation locData)) <$> singlePackageLocations
    let allResults = (++) singlePackageRegistries <$> mapM sequence fullRepositoryFetchResults
    case allResults of
      Left err -> pure $ Left err
      Right registries -> do
        let path = Stuff.registry cache
        let registry = Map.fromList registries
        File.writeBinary path (zelmRegistriesFromRegistriesMap registry)
        pure $ Right (zelmRegistriesFromRegistriesMap registry)


createRegistryFromSinglePackageLocation :: SinglePackageLocationData -> Registry
createRegistryFromSinglePackageLocation SinglePackageLocationData{_packageName=packageName, _version=version} =
    Registry {_count = 0, _versions = Map.singleton packageName KnownVersions {_newest = version, _previous=[]}}


fetchSingleCustomRepository :: Http.Manager -> CustomSingleRepositoryData -> IO (Either Exit.RegistryProblem Registry)
fetchSingleCustomRepository manager (CustomSingleRepositoryData{_repositoryType=repositoryType, _repositoryUrl=repositoryUrl}) =
  case repositoryType of
    DefaultPackageServer -> fetchFromRepositoryUrl manager repositoryUrl
    -- FIXME
    BarebonesPackageServer -> error "not yet implemented"


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

update :: Http.Manager -> Stuff.ZelmSpecificCache -> ZelmRegistries -> IO (Either Exit.RegistryProblem ZelmRegistries)
update manager cache zelmRegistries =
  do
    let registriesMap = _registries zelmRegistries
    let listOfProblemsOrKeyRegistryPairs = traverse (\(k, v) -> fmap (fmap ((,) k)) (updateSingleRegistry manager k v)) (Map.toList registriesMap)
    newRegistryOrError <- sequence <$> listOfProblemsOrKeyRegistryPairs
    let newRegistryOrError' = fmap Map.fromList newRegistryOrError
    case newRegistryOrError' of
      Left err -> pure $ Left err
      Right newRegistry ->
        do
          -- FIXME: There's gotta be a faster way of doing this
          let newZelmRegistries = zelmRegistriesFromRegistriesMap newRegistry
          _ <- File.writeBinary (Stuff.registry cache) newZelmRegistries
          pure $ Right newZelmRegistries


  -- = RepositoryUrlKey RepositoryUrl
  -- | PackageUrlKey PackageUrl
  -- deriving (Eq, Ord)
updateSingleRegistry :: Http.Manager -> RegistryKey -> Registry -> IO (Either Exit.RegistryProblem Registry)
updateSingleRegistry manager registryKey registry =
  case registryKey of
    -- FIXME: need to deal with bare repos
    RepositoryUrlKey repositoryUrl -> updateSingleRegistryFromStandardElmRepo manager repositoryUrl registry
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

-- FIXME: This needs to be fixed to allow barebones servers
customSingleRepositoryDataToRegistryKey :: CustomSingleRepositoryData -> RegistryKey
customSingleRepositoryDataToRegistryKey CustomSingleRepositoryData{_repositoryUrl=repositoryUrl} = RepositoryUrlKey repositoryUrl

--FIXME: PackageUrlKey probably needs to include the kind of package it is for decompression reasons
singlePackageLocationDataToRegistryKey :: SinglePackageLocationData -> RegistryKey
singlePackageLocationDataToRegistryKey SinglePackageLocationData{_url=url}= PackageUrlKey url

doesRegistryAgreeWithCustomRepositoriesData :: CustomRepositoriesData -> ZelmRegistries -> Bool
doesRegistryAgreeWithCustomRepositoriesData (CustomRepositoriesData fullRepositories singlePackages) registry =
  Set.fromList allRegistryKeys == Map.keysSet (_registries registry)
    where
      allRegistryKeys = (customSingleRepositoryDataToRegistryKey <$> fullRepositories) ++ (singlePackageLocationDataToRegistryKey <$> singlePackages)

latest :: Http.Manager -> CustomRepositoriesData -> Stuff.ZelmSpecificCache -> IO (Either Exit.RegistryProblem ZelmRegistries)
latest manager customRepositoriesData cache =
  do
    maybeOldRegistry <- read cache
    case maybeOldRegistry of
      Just oldRegistry -> if doesRegistryAgreeWithCustomRepositoriesData customRepositoriesData oldRegistry
        then update manager cache oldRegistry
        -- FIXME: This is too conservative
        else fetch manager cache customRepositoriesData
      Nothing -> fetch manager cache customRepositoriesData



-- GET VERSIONS

compareVersionToKnownVersions :: V.Version -> Maybe KnownVersions -> KnownVersions
compareVersionToKnownVersions version knownVersionsMaybe =
  case knownVersionsMaybe of
    Just (KnownVersions newest others) -> KnownVersions (max newest version) (min newest version : others)
    Nothing -> KnownVersions version []


versionsToKnownVersions :: [V.Version] -> Maybe KnownVersions
versionsToKnownVersions = foldr (\v acc -> Just $ compareVersionToKnownVersions v acc) Nothing


getVersions :: Pkg.Name -> ZelmRegistries -> Maybe KnownVersions
getVersions name ZelmRegistries{_packagesToLocations=packagesToLocations} =
  do
    versionsMap <- Map.lookup name packagesToLocations
    let versions = Map.keys versionsMap
    versionsToKnownVersions versions

getVersions' :: Pkg.Name -> ZelmRegistries -> Either [Pkg.Name] KnownVersions
getVersions' name zelmRegistry =
  case getVersions name zelmRegistry of
    Just kvs -> Right kvs
    -- FIXME: Maybe a faster way than just brute-force merging?
    Nothing -> Left $ Pkg.nearbyNames name (Map.keys (_versions $ mergeRegistries zelmRegistry))



-- POST


post :: Http.Manager -> RepositoryUrl -> String -> D.Decoder x a -> (a -> IO b) -> IO (Either Exit.RegistryProblem b)
post manager repositoryUrl path decoder callback =
  let
    url = Website.route repositoryUrl path []
  in
  Http.post manager url [] Exit.RP_Http $
    \body ->
      case D.fromByteString decoder body of
        Right a -> Right <$> callback a
        Left _ -> return $ Left $ Exit.RP_Data url body



-- BINARY

instance Binary RegistryKey where
  get = do
    t <- get :: Get Word8
    case t of
      0 -> do
        repositoryUrl <- get :: Get RepositoryUrl
        pure $ RepositoryUrlKey repositoryUrl
      1 -> do
        packageUrl <- get :: Get PackageUrl
        pure $ PackageUrlKey packageUrl

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


instance Binary ZelmRegistries where
  get = do
    registries <- get :: Get (Map.Map RegistryKey Registry)
    packagesToLocations <- get :: Get (Map.Map Pkg.Name (Map.Map V.Version RegistryKey))
    pure $ ZelmRegistries{_registries=registries, _packagesToLocations=packagesToLocations}

  put (ZelmRegistries registries packagesToLocations) = do
    put registries
    put packagesToLocations