{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Elm.CustomRepositoryData
  ( CustomSingleRepositoryData(..)
  , CustomRepositoriesData(..)
  , SinglePackageLocationData(..)
  , RepositoryType(..)
  , RepositoryUrl
  , PackageUrl
  , SinglePackageFileType(..)
  , customRepostoriesDataDecoder
  , customRepostoriesDataEncoder
  , defaultCustomRepositoriesData
  , defaultCustomRepositoriesDataElmPackageRepoOnly
  , CustomRepositoryDataParseError(..)
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
data PACKAGEURL

data RepositoryType
  = DefaultPackageServer
  | BarebonesPackageServer
  deriving (Enum, Bounded)

allRepositoryTypes :: [RepositoryType]
allRepositoryTypes = [(minBound :: RepositoryType) .. ]

defaultPackageServerString :: Json.String
defaultPackageServerString = Json.fromChars "package-server-with-standard-elm-package-server-api"

barebonesPackageServerString :: Json.String
barebonesPackageServerString = Json.fromChars "package-server-with-barebones-api"

repositoryTypeToString :: RepositoryType -> Json.String
repositoryTypeToString repositoryType =
  case repositoryType of
    DefaultPackageServer -> defaultPackageServerString
    BarebonesPackageServer -> barebonesPackageServerString

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
repositoryTypeEncoder BarebonesPackageServer = E.string barebonesPackageServerString

type RepositoryUrl = Utf8.Utf8 REPOSITORYURL

instance Binary.Binary (Utf8.Utf8 REPOSITORYURL) where
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


data CustomSingleRepositoryData =
  CustomSingleRepositoryData
    { _repositoryType :: !RepositoryType
    , _repositoryUrl :: !RepositoryUrl
    }

standardElmRepository :: CustomSingleRepositoryData
standardElmRepository = CustomSingleRepositoryData
  { _repositoryType = DefaultPackageServer
  , _repositoryUrl = Utf8.fromChars "https://package.elm-lang.org"
  }

standardZokkaRepository :: CustomSingleRepositoryData
standardZokkaRepository = CustomSingleRepositoryData
  { _repositoryType = DefaultPackageServer
  , _repositoryUrl = Utf8.fromChars "https://package-server.zokka-lang.com"
  }

customSingleRepositoryDataDecoder :: D.Decoder CustomRepositoryDataParseError CustomSingleRepositoryData
customSingleRepositoryDataDecoder =
  do
    repositoryType <- D.field "repository-type" (repositoryTypeDecoder UnsupportedRepositoryType)
    repositoryUrl <- D.field "repository-url" repositoryUrlDecoder
    pure (CustomSingleRepositoryData{_repositoryType=repositoryType, _repositoryUrl=repositoryUrl})

customSingleRepositoryDataEncoder :: CustomSingleRepositoryData -> E.Value
customSingleRepositoryDataEncoder (CustomSingleRepositoryData repositoryType repositoryUrl) =
  E.object
    [ (Utf8.fromChars "repository-type", repositoryTypeEncoder repositoryType)
    , (Utf8.fromChars "repository-url", repositoryUrlEncoder repositoryUrl)
    ]

data SinglePackageFileType
  = TarballType
  | ZipfileType
  deriving (Enum, Bounded)

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


humanReadableShaDigestToString :: HumanReadableShaDigest -> String
humanReadableShaDigestToString (HumanReadableShaDigest jsonString) = Utf8.toChars jsonString


humanReadableShaDigestToJsonString :: HumanReadableShaDigest -> Json.String
humanReadableShaDigestToJsonString (HumanReadableShaDigest shaDigest) = shaDigest


shaToHumanReadableShaDigest :: Sha -> HumanReadableShaDigest
shaToHumanReadableShaDigest = HumanReadableShaDigest . Utf8.fromChars . Http.shaToChars


data SinglePackageLocationData =
  SinglePackageLocationData
    { _fileType :: !SinglePackageFileType
    , _packageName :: !Name
    , _version :: !Version
    , _url :: !PackageUrl
    , _shaHash :: !HumanReadableShaDigest
    }

data CustomRepositoryDataParseError
  -- UnsupportedFileTypeError, first string is the string that a user tried to input as a file type, list of strings are suggestions of syntactically close filetypes
  = UnsupportedFileTypeError Json.String [Json.String]
  | InvalidVersionString (Row, Col)
  | InvalidPackageName (Row, Col)
  | UnsupportedRepositoryType Json.String [Json.String]
  | InvalidHashType Json.String [Json.String]
  deriving Show


sha1HashTypeString :: Json.String
sha1HashTypeString = Utf8.fromChars "sha-1"


singlePackageLocationDataDecoder :: D.Decoder CustomRepositoryDataParseError SinglePackageLocationData
singlePackageLocationDataDecoder =
  do
    fileType <- D.field "file-type" (singlePackageFileTypeDecoder UnsupportedFileTypeError)
    packageName <- D.field "package-name" (D.mapError InvalidVersionString Elm.Package.decoder)
    version <- D.field "version" (D.mapError InvalidPackageName Elm.Version.decoder)
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
