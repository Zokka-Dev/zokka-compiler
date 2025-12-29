{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Elm.CustomRepositoryData
  ( CustomSingleRepositoryData(..)
  , DefaultPackageServerRepo(..)
  , PZRPackageServerRepo(..)
  , CustomRepositoriesData(..)
  , SinglePackageLocationData(..)
  , LocalReadOnlyMirrorRepo(..)
  , RepositoryType(..)
  , RepositoryUrl
  , RepositoryAuthToken
  , RepositoryLocalName
  , RepositoryFilePath
  , RepositoryLocation(..)
  , PackageUrl
  , SinglePackageFileType(..)
  , customRepostoriesDataDecoder
  , customRepostoriesDataEncoder
  , defaultCustomRepositoriesData
  , defaultCustomRepositoriesDataElmPackageRepoOnly
  , CustomRepositoryDataParseError(..)
  , HumanReadableShaDigest
  , humanReadableShaDigestIsEqualToSha
  , humanReadableShaDigestToString
  , shaToHumanReadableShaDigest
  )
  where

import Elm.Version (Version, decoder, encode)
import qualified Data.Utf8 as Utf8
import Elm.Package (Name, decoder, encode)
import Network.HTTP (RequestMethod(Custom))
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String as Json
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import Data.Coerce (coerce)
import Parse.Primitives (Col, Row)
import Http (Sha, shaToChars)
import Control.Monad (when)

data REPOSITORYURL
data REPOSITORYFILEPATH
data PACKAGEURL
data REPOSITORYAUTHTOKEN
data REPOSITORYLOCALNAME

data RepositoryType
  = DefaultPackageServer
  | PZRPackageServer
  | LocalReadOnlyMirrorFileBundle
  deriving (Enum, Bounded, Show, Ord, Eq)


data DefaultPackageServerRepo = DefaultPackageServerRepo
  { _defaultPackageServerRepoTypeUrl :: !RepositoryUrl
  , _defaultPackageServerRepoLocalName :: !RepositoryLocalName
  }
    deriving (Show, Ord, Eq)

type RepositoryAuthToken = Utf8.Utf8 REPOSITORYAUTHTOKEN

instance Binary.Binary (Utf8.Utf8 REPOSITORYAUTHTOKEN) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong

-- PZR stands for personal Zokka repo
data PZRPackageServerRepo = PZRPackageServerRepo
  { _pzrPackageServerRepoTypeUrl :: !RepositoryUrl
  , _pzrPackageServerRepoAuthToken :: !RepositoryAuthToken
  , _pzrPackageServerRepoLocalName :: !RepositoryLocalName
  }
    deriving (Show, Ord, Eq)

-- Note that we don't have a local name because this is a read-only repo that we
-- cannot publish to. The reason we can't publish to it is because this is meant
-- to be synchronized with the Elm package server which relies on counting the
-- total number of packages to find the right number to send to "since".
--
-- This is solvable in theory (we can treat user-created packages differently
-- than ones synced from the Elm package server), but it seems a cleaner
-- solution is to just offer a different kind of repo altogether for user
-- created packages. At the moment we require a local package server to do that,
-- but if there's demand we can also have a version that just makes a series of
-- files without the need for a server.
data LocalReadOnlyMirrorRepo = LocalReadOnlyMirrorRepo
  { _localReadOnlyMirrorRepoFilePath :: !RepositoryFilePath
  }
    deriving (Show, Ord, Eq)

allRepositoryTypes :: [RepositoryType]
allRepositoryTypes = [(minBound :: RepositoryType) .. ]

defaultPackageServerString :: Json.String
defaultPackageServerString = Json.fromChars "package-server-with-standard-elm-v0.19-package-server-api"

pzrPackageServerString :: Json.String
pzrPackageServerString = Json.fromChars "package-server-with-personal-zokka-repo-v1.0-package-server-api"

localReadOnlyMirrorFileBundleString :: Json.String
localReadOnlyMirrorFileBundleString = Json.fromChars "local-directory-of-files-readonly-mirror-of-package-server"

repositoryTypeToString :: RepositoryType -> Json.String
repositoryTypeToString repositoryType =
  case repositoryType of
    DefaultPackageServer -> defaultPackageServerString
    PZRPackageServer -> pzrPackageServerString
    LocalReadOnlyMirrorFileBundle -> localReadOnlyMirrorFileBundleString

allRepositoryTypeStrings :: [Json.String]
allRepositoryTypeStrings = fmap repositoryTypeToString allRepositoryTypes

allRepositoryTypesLookupMap :: Map.Map Json.String RepositoryType
allRepositoryTypesLookupMap = Map.fromList repositoryTypeStringPairs
  where
    repositoryTypeStringPairs = fmap (\r -> (repositoryTypeToString r, r)) allRepositoryTypes

lookupRepositoryType :: Json.String -> Either [Json.String] RepositoryType
lookupRepositoryType rawTypeStr =
  case Map.lookup rawTypeStr allRepositoryTypesLookupMap of
    Just repositoryType -> Right repositoryType
    -- FIXME: See https://github.com/changlinli/zokka-compiler/issues/1
    Nothing -> Left allRepositoryTypeStrings

repositoryTypeDecoder :: (Json.String -> [Json.String] -> e) -> D.Decoder e RepositoryType
repositoryTypeDecoder toError =
  do
    str <- D.string
    case lookupRepositoryType str of
      Right repositoryType -> pure repositoryType
      Left suggestions -> D.failure (toError str suggestions)

repositoryTypeEncoder :: RepositoryType -> E.Value
repositoryTypeEncoder DefaultPackageServer = E.string defaultPackageServerString
repositoryTypeEncoder PZRPackageServer = E.string pzrPackageServerString
repositoryTypeEncoder LocalReadOnlyMirrorFileBundle = E.string localReadOnlyMirrorFileBundleString

type RepositoryLocalName = Utf8.Utf8 REPOSITORYLOCALNAME

instance Binary.Binary (Utf8.Utf8 REPOSITORYLOCALNAME) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong


repositoryLocalNameDecoder :: D.Decoder e RepositoryLocalName
repositoryLocalNameDecoder = fmap coerce D.string


repositoryLocalNameEncoder :: RepositoryLocalName -> E.Value
repositoryLocalNameEncoder repositoryLocalName = E.string (coerce repositoryLocalName)

repositoryFilePathEncoder :: RepositoryFilePath -> E.Value
repositoryFilePathEncoder repositoryFilePath = E.string (coerce repositoryFilePath)

repositoryFilePathDecoder :: D.Decoder e RepositoryFilePath
repositoryFilePathDecoder = fmap coerce D.string

data RepositoryLocation 
  = RemoteRepository RepositoryUrl 
  | LocalFileSystemRepository RepositoryFilePath

type RepositoryUrl = Utf8.Utf8 REPOSITORYURL

instance Binary.Binary (Utf8.Utf8 REPOSITORYURL) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong

type RepositoryFilePath = Utf8.Utf8 REPOSITORYFILEPATH

instance Binary.Binary (Utf8.Utf8 REPOSITORYFILEPATH) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong


repositoryUrlDecoder :: D.Decoder e RepositoryUrl
repositoryUrlDecoder = fmap coerce D.string

repositoryUrlEncoder :: RepositoryUrl -> E.Value
repositoryUrlEncoder repositoryUrl = E.string (coerce repositoryUrl)


type PackageUrl = Utf8.Utf8 PACKAGEURL

instance Binary.Binary (Utf8.Utf8 PACKAGEURL) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong


packageUrlDecoder :: D.Decoder e PackageUrl
packageUrlDecoder = fmap coerce D.string


packageUrlEncoder :: PackageUrl -> E.Value
packageUrlEncoder packageUrl = E.string (coerce packageUrl)


sha1Decoder :: D.Decoder e HumanReadableShaDigest
sha1Decoder = fmap coerce D.string


sha1Encoder :: HumanReadableShaDigest -> E.Value
sha1Encoder (HumanReadableShaDigest shaDigest) = E.string shaDigest


data CustomSingleRepositoryData 
  = DefaultPackageServerRepoData DefaultPackageServerRepo
  | PZRPackageServerRepoData PZRPackageServerRepo
  | LocalReadOnlyMirrorRepoData LocalReadOnlyMirrorRepo
    deriving (Show, Ord, Eq)

standardElmRepositoryDefaultPackageServerRepo :: DefaultPackageServerRepo
standardElmRepositoryDefaultPackageServerRepo = DefaultPackageServerRepo
  { _defaultPackageServerRepoTypeUrl = Utf8.fromChars "https://package.elm-lang.org"
  , _defaultPackageServerRepoLocalName = Utf8.fromChars "standard-elm-repository"
  }

standardElmRepository :: CustomSingleRepositoryData
standardElmRepository = DefaultPackageServerRepoData standardElmRepositoryDefaultPackageServerRepo

standardZokkaRepositoryDefaultPackageServerRepo :: DefaultPackageServerRepo
standardZokkaRepositoryDefaultPackageServerRepo = DefaultPackageServerRepo
  { _defaultPackageServerRepoTypeUrl = Utf8.fromChars "https://package-server.zokka-lang.com"
  , _defaultPackageServerRepoLocalName = Utf8.fromChars "standard-zokka-repository"
  }

standardZokkaRepository :: CustomSingleRepositoryData
standardZokkaRepository = DefaultPackageServerRepoData standardZokkaRepositoryDefaultPackageServerRepo

repositoryAuthTokenDecoder :: D.Decoder e RepositoryAuthToken
repositoryAuthTokenDecoder = fmap coerce D.string

repositoryAuthTokenEncoder :: RepositoryAuthToken -> E.Value
repositoryAuthTokenEncoder authToken = E.string (coerce authToken)

customSingleRepositoryDataDecoder :: D.Decoder CustomRepositoryDataParseError CustomSingleRepositoryData
customSingleRepositoryDataDecoder =
  do
    repositoryType <- D.field "repository-type" (repositoryTypeDecoder UnsupportedRepositoryType)
    case repositoryType of
      DefaultPackageServer ->
        do
          repositoryUrl <- D.field "repository-url" repositoryUrlDecoder
          repositoryLocalName <- D.field "repository-local-name" repositoryLocalNameDecoder
          pure (DefaultPackageServerRepoData (DefaultPackageServerRepo{_defaultPackageServerRepoTypeUrl=repositoryUrl, _defaultPackageServerRepoLocalName=repositoryLocalName}))
      PZRPackageServer ->
        do
          repositoryUrl <- D.field "repository-url" repositoryUrlDecoder
          repositoryLocalName <- D.field "repository-local-name" repositoryLocalNameDecoder
          repositoryAuthToken <- D.field "repository-auth-token" repositoryAuthTokenDecoder
          pure (PZRPackageServerRepoData (PZRPackageServerRepo {_pzrPackageServerRepoAuthToken=repositoryAuthToken, _pzrPackageServerRepoTypeUrl=repositoryUrl, _pzrPackageServerRepoLocalName=repositoryLocalName}))
      LocalReadOnlyMirrorFileBundle ->
        do
          repositoryFilePath <- D.field "repository-file-path" repositoryFilePathDecoder
          pure (LocalReadOnlyMirrorRepoData (LocalReadOnlyMirrorRepo {_localReadOnlyMirrorRepoFilePath=repositoryFilePath}))


customSingleRepositoryDataEncoder :: CustomSingleRepositoryData -> E.Value
customSingleRepositoryDataEncoder customSingleRepositoryData =
  case customSingleRepositoryData of
    DefaultPackageServerRepoData defaultPackageServerRepo ->
      E.object
        [ (Utf8.fromChars "repository-type", repositoryTypeEncoder DefaultPackageServer)
        , (Utf8.fromChars "repository-url", repositoryUrlEncoder (_defaultPackageServerRepoTypeUrl defaultPackageServerRepo))
        , (Utf8.fromChars "repository-local-name", repositoryLocalNameEncoder (_defaultPackageServerRepoLocalName defaultPackageServerRepo))
        ]
    PZRPackageServerRepoData pzrPackageServerRepo -> 
      E.object
        [ (Utf8.fromChars "repository-type", repositoryTypeEncoder PZRPackageServer)
        , (Utf8.fromChars "repository-url", repositoryUrlEncoder (_pzrPackageServerRepoTypeUrl pzrPackageServerRepo))
        , (Utf8.fromChars "repository-local-name", repositoryLocalNameEncoder (_pzrPackageServerRepoLocalName pzrPackageServerRepo))
        , (Utf8.fromChars "repository-auth-token", repositoryAuthTokenEncoder (_pzrPackageServerRepoAuthToken pzrPackageServerRepo))
        ]
    LocalReadOnlyMirrorRepoData localReadOnlyMirrorRepo ->
      E.object
        [ (Utf8.fromChars "repository-type", repositoryTypeEncoder LocalReadOnlyMirrorFileBundle)
        , (Utf8.fromChars "repository-file-path", repositoryFilePathEncoder (_localReadOnlyMirrorRepoFilePath localReadOnlyMirrorRepo))
        -- Note that there is no local name because we can't publish to this kind of repository
        ]

data SinglePackageFileType
  = TarballType
  | ZipfileType
  deriving (Enum, Bounded, Show, Ord, Eq)

tarballTypeString :: Json.String
tarballTypeString = Json.fromChars "tarball"

zipfileTypeString :: Json.String
zipfileTypeString = Json.fromChars "zipfile"

allSinglePackageFileTypes :: [SinglePackageFileType]
allSinglePackageFileTypes = [(minBound :: SinglePackageFileType) .. ]

singlePackageFileTypeToString :: SinglePackageFileType -> Json.String
singlePackageFileTypeToString singlePackageFileType =
  case singlePackageFileType of
    TarballType -> tarballTypeString
    ZipfileType -> zipfileTypeString

allSinglePackageFileTypeStrings :: [Json.String]
allSinglePackageFileTypeStrings = fmap singlePackageFileTypeToString allSinglePackageFileTypes

allSinglePackageFileTypesLookupMap :: Map.Map Json.String SinglePackageFileType
allSinglePackageFileTypesLookupMap = Map.fromList singlePackageFileTypePairs
  where
    singlePackageFileTypePairs = fmap (\p -> (singlePackageFileTypeToString p, p)) allSinglePackageFileTypes

singlePackageFileTypeLookup :: Json.String -> Either [Json.String] SinglePackageFileType
singlePackageFileTypeLookup string =
  case Map.lookup string allSinglePackageFileTypesLookupMap of
    Just singlePackageFileType -> Right singlePackageFileType
    Nothing -> Left allSinglePackageFileTypeStrings

singlePackageFileTypeDecoder :: (Json.String -> [Json.String] -> e) -> D.Decoder e SinglePackageFileType
singlePackageFileTypeDecoder toError =
  do
    string <- D.string
    case singlePackageFileTypeLookup string of
      Right singlePackageFileType -> pure singlePackageFileType
      Left suggestions -> D.failure (toError string suggestions)

singlePackageFileTypeEncoder :: SinglePackageFileType -> E.Value
singlePackageFileTypeEncoder TarballType = E.string tarballTypeString
singlePackageFileTypeEncoder ZipfileType = E.string zipfileTypeString


newtype HumanReadableShaDigest = HumanReadableShaDigest Json.String
  deriving (Eq, Ord, Show)


humanReadableShaDigestToString :: HumanReadableShaDigest -> String
humanReadableShaDigestToString (HumanReadableShaDigest jsonString) = Utf8.toChars jsonString


humanReadableShaDigestToJsonString :: HumanReadableShaDigest -> Json.String
humanReadableShaDigestToJsonString (HumanReadableShaDigest shaDigest) = shaDigest


shaToHumanReadableShaDigest :: Sha -> HumanReadableShaDigest
shaToHumanReadableShaDigest = HumanReadableShaDigest . Utf8.fromChars . Http.shaToChars


humanReadableShaDigestIsEqualToSha :: HumanReadableShaDigest -> Sha -> Bool
humanReadableShaDigestIsEqualToSha (HumanReadableShaDigest shaDigest) sha =
  Utf8.toChars shaDigest == shaToChars sha


data SinglePackageLocationData =
  SinglePackageLocationData
    { _fileType :: !SinglePackageFileType
    , _packageName :: !Name
    , _version :: !Version
    , _url :: !PackageUrl
    , _shaHash :: !HumanReadableShaDigest
    }
  deriving (Eq, Ord, Show)

data CustomRepositoryDataParseError
  -- UnsupportedFileTypeError, first string is the string that a user tried to input as a file type, list of strings are suggestions of syntactically close filetypes
  = UnsupportedFileTypeError Json.String [Json.String]
  | InvalidVersionString (Row, Col)
  | InvalidPackageName (Row, Col)
  | UnsupportedRepositoryType Json.String [Json.String]
  | InvalidHashType Json.String [Json.String]
  deriving (Eq, Ord, Show)


sha1HashTypeString :: Json.String
sha1HashTypeString = Utf8.fromChars "sha-1"


singlePackageLocationDataDecoder :: D.Decoder CustomRepositoryDataParseError SinglePackageLocationData
singlePackageLocationDataDecoder =
  do
    fileType <- D.field "file-type" (singlePackageFileTypeDecoder UnsupportedFileTypeError)
    packageName <- D.field "package-name" (D.mapError InvalidPackageName Elm.Package.decoder)
    version <- D.field "version" (D.mapError InvalidVersionString Elm.Version.decoder)
    url <- D.field "url" packageUrlDecoder
    hashType <- D.field "hash-type" D.string
    when (hashType /= sha1HashTypeString) $ D.failure (InvalidHashType hashType [sha1HashTypeString])
    hash <- D.field "hash" sha1Decoder
    pure $
      SinglePackageLocationData
        { _fileType=fileType
        , _packageName=packageName
        , _version=version
        , _url=url
        , _shaHash=hash
        }

singlePackageLocationDataEncoder :: SinglePackageLocationData -> E.Value
singlePackageLocationDataEncoder (SinglePackageLocationData fileType packageName version url shaHash) =
  E.object
    [ (Utf8.fromChars "file-type", singlePackageFileTypeEncoder fileType)
    , (Utf8.fromChars "package-name", Elm.Package.encode packageName)
    , (Utf8.fromChars "version", Elm.Version.encode version)
    , (Utf8.fromChars "url", packageUrlEncoder url)
    , (Utf8.fromChars "hash-type", E.string sha1HashTypeString)
    , (Utf8.fromChars "hash", E.string . humanReadableShaDigestToJsonString $ shaHash)
    ]

data CustomRepositoriesData =
  CustomRepositoriesData
    { _customFullRepositories :: [CustomSingleRepositoryData]
    , _customSinglePackageRepositories :: [SinglePackageLocationData]
    }

customRepostoriesDataDecoder :: D.Decoder CustomRepositoryDataParseError CustomRepositoriesData
customRepostoriesDataDecoder = do
  customFullRepositories <- D.field "repositories" (D.list customSingleRepositoryDataDecoder)
  customSinglePackageRepositories <- D.field "single-package-locations" (D.list singlePackageLocationDataDecoder)
  pure $
    CustomRepositoriesData
      { _customFullRepositories=customFullRepositories
      , _customSinglePackageRepositories=customSinglePackageRepositories
      }

defaultCustomRepositoriesDataElmPackageRepoOnly :: CustomRepositoriesData
defaultCustomRepositoriesDataElmPackageRepoOnly = CustomRepositoriesData
  { _customFullRepositories =
    [ standardElmRepository
    ]
  , _customSinglePackageRepositories = []
  }

defaultCustomRepositoriesData :: CustomRepositoriesData
defaultCustomRepositoriesData = CustomRepositoriesData
  { _customFullRepositories =
    [ standardElmRepository
    , standardZokkaRepository
    ]
  , _customSinglePackageRepositories = []
  }

customRepostoriesDataEncoder :: CustomRepositoriesData -> E.Value
customRepostoriesDataEncoder (CustomRepositoriesData customFullRepositories customSinglePackageRepositories) =
  E.object
    [ (Utf8.fromChars "repositories", E.list customSingleRepositoryDataEncoder customFullRepositories)
    , (Utf8.fromChars "single-package-locations", E.list singlePackageLocationDataEncoder customSinglePackageRepositories)
    ]

instance Binary.Binary RepositoryType where
  get = do
    t <- Binary.get :: Binary.Get Binary.Word8
    case t of
      0 -> pure DefaultPackageServer
      1 -> pure PZRPackageServer
      _ -> 
        -- FIXME: Better error message
        error "Corrupt repository type! We should only have a 0 or 1 here."
  put repositoryType = case repositoryType of
    DefaultPackageServer -> Binary.put (0 :: Binary.Word8)
    PZRPackageServer -> Binary.put (1 :: Binary.Word8)
    LocalReadOnlyMirrorFileBundle -> Binary.put (2 :: Binary.Word8)

instance Binary.Binary CustomSingleRepositoryData where
  get = do
    repositoryType <- Binary.get :: Binary.Get RepositoryType
    case repositoryType of
      DefaultPackageServer ->
        do
          repositoryUrl <- Binary.get :: Binary.Get RepositoryUrl
          repositoryLocalName <- Binary.get :: Binary.Get RepositoryLocalName
          pure (DefaultPackageServerRepoData (DefaultPackageServerRepo {_defaultPackageServerRepoTypeUrl=repositoryUrl, _defaultPackageServerRepoLocalName=repositoryLocalName}))
      PZRPackageServer ->
        do
          repositoryUrl <- Binary.get :: Binary.Get RepositoryUrl
          repositoryLocalName <- Binary.get :: Binary.Get RepositoryLocalName
          repositoryAuthToken <- Binary.get :: Binary.Get RepositoryAuthToken
          pure (PZRPackageServerRepoData (PZRPackageServerRepo {_pzrPackageServerRepoAuthToken=repositoryAuthToken, _pzrPackageServerRepoTypeUrl=repositoryUrl, _pzrPackageServerRepoLocalName=repositoryLocalName}))
      LocalReadOnlyMirrorFileBundle ->
        do
          repositoryFilePath <- Binary.get :: Binary.Get RepositoryFilePath
          pure (LocalReadOnlyMirrorRepoData (LocalReadOnlyMirrorRepo {_localReadOnlyMirrorRepoFilePath=repositoryFilePath}))

  put customSingleRepositoryData =
    case customSingleRepositoryData of
      DefaultPackageServerRepoData defaultPackageServerRepo ->
        do
          Binary.put DefaultPackageServer
          Binary.put (_defaultPackageServerRepoTypeUrl defaultPackageServerRepo)
          Binary.put (_defaultPackageServerRepoLocalName defaultPackageServerRepo)
      PZRPackageServerRepoData pzrPackageServer ->
        do
          Binary.put PZRPackageServer
          Binary.put (_pzrPackageServerRepoTypeUrl pzrPackageServer)
          Binary.put (_pzrPackageServerRepoLocalName pzrPackageServer)
          Binary.put (_pzrPackageServerRepoAuthToken pzrPackageServer)
      LocalReadOnlyMirrorRepoData localReadOnlyMirrorRepo ->
        do
          Binary.put LocalReadOnlyMirrorFileBundle
          Binary.put (_localReadOnlyMirrorRepoFilePath localReadOnlyMirrorRepo)

  -- = TarballType
  -- | ZipfileType
instance Binary.Binary SinglePackageFileType where
  get = do
    t <- Binary.get :: Binary.Get Binary.Word8
    case t of
      0 -> pure TarballType
      1 -> pure ZipfileType
      _ -> 
        -- FIXME: Better error message
        error "Corrupt SinglePackageFileType! We should only have a 0 or 1 here."
  
  put singlePackageFileType = case singlePackageFileType of
    TarballType -> Binary.put (0 :: Binary.Word8)
    ZipfileType -> Binary.put (1 :: Binary.Word8)

instance Binary.Binary HumanReadableShaDigest where
  get = do
    -- FIXME: This is really hacky to use a PackageURL
    shaDigestAsUtf8String <- Binary.get :: Binary.Get PackageUrl
    pure (HumanReadableShaDigest (coerce shaDigestAsUtf8String))

  put (HumanReadableShaDigest shaDigestAsUtf8String) = do
    Binary.put (coerce shaDigestAsUtf8String :: PackageUrl)


instance Binary.Binary SinglePackageLocationData where
  get = do
    fileType <- Binary.get :: Binary.Get SinglePackageFileType
    packageName <- Binary.get :: Binary.Get Name
    version <- Binary.get :: Binary.Get Version
    url <- Binary.get :: Binary.Get PackageUrl
    shaHash <- Binary.get :: Binary.Get HumanReadableShaDigest
    pure $ SinglePackageLocationData
      { _fileType = fileType
      , _packageName = packageName
      , _version = version
      , _url = url
      , _shaHash = shaHash
      }

  put SinglePackageLocationData
      { _fileType = fileType
      , _packageName = packageName
      , _version = version
      , _url = url
      , _shaHash = shaHash
      }
      = do
        Binary.put fileType
        Binary.put packageName
        Binary.put version
        Binary.put url
        Binary.put shaHash