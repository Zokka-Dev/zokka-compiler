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
  )
  where

import Elm.Version (Version, decoder)
import qualified Data.Utf8 as Utf8
import Elm.Package (Name, decoder)
import Network.HTTP (RequestMethod(Custom))
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String as Json
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import Data.Coerce (coerce)

data REPOSITORYURL
data PACKAGEURL

data RepositoryType
  = DefaultPackageServer
  | BarebonesPackageServer
  deriving (Enum, Bounded)

allRepositoryTypes :: [RepositoryType]
allRepositoryTypes = [(minBound :: RepositoryType) .. ]

defaultPackageServerString :: Json.String
defaultPackageServerString = Json.fromChars "default-package-server"

barebonesPackageServerString :: Json.String
barebonesPackageServerString = Json.fromChars "barebones-package-server"

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
    -- FIXME: See https://github.com/changlinli/zelm-compiler/issues/1
    Nothing -> Left allRepositoryTypeStrings

repositoryTypeDecoder :: (Json.String -> [Json.String] -> e) -> D.Decoder e RepositoryType
repositoryTypeDecoder toError =
  do
    str <- D.string
    case lookupRepositoryType str of
      Right repositoryType -> pure repositoryType
      Left suggestions -> D.failure (toError str suggestions)

type RepositoryUrl = Utf8.Utf8 REPOSITORYURL

instance Binary.Binary (Utf8.Utf8 REPOSITORYURL) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong


repositoryUrlDecoder :: D.Decoder e RepositoryUrl
repositoryUrlDecoder = fmap coerce D.string


type PackageUrl = Utf8.Utf8 PACKAGEURL

instance Binary.Binary (Utf8.Utf8 PACKAGEURL) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong


packageUrlDecoder :: D.Decoder e PackageUrl
packageUrlDecoder = fmap coerce D.string


data CustomSingleRepositoryData =
  CustomSingleRepositoryData
    { _repositoryType :: !RepositoryType
    , _repositoryUrl :: !RepositoryUrl
    }

customSingleRepositoryDataDecoder :: (Json.String -> [Json.String] -> e) -> D.Decoder e CustomSingleRepositoryData
customSingleRepositoryDataDecoder toError =
  do
    repositoryType <- D.field "repository-type" (repositoryTypeDecoder toError)
    repositoryUrl <- D.field "repository-url" repositoryUrlDecoder
    pure (CustomSingleRepositoryData{_repositoryType=repositoryType, _repositoryUrl=repositoryUrl})

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

data SinglePackageLocationData =
  SinglePackageLocationData
    { _fileType :: !SinglePackageFileType
    , _packageName :: !Name
    , _version :: !Version
    , _url :: !PackageUrl
    }

singlePackageLocationDataDecoder :: (Json.String -> [Json.String] -> e) -> D.Decoder e SinglePackageLocationData
singlePackageLocationDataDecoder toErr =
  do
    fileType <- singlePackageFileTypeDecoder toErr
    packageName <- D.mapError undefined Elm.Package.decoder
    version <- D.mapError undefined Elm.Version.decoder
    url <- packageUrlDecoder
    pure $
      SinglePackageLocationData
        { _fileType=fileType
        , _packageName=packageName
        , _version=version
        , _url=url
        }

data CustomRepositoriesData =
  CustomRepositoriesData
    { _customFullRepositories :: [CustomSingleRepositoryData]
    , _customSinglePackageRepositories :: [SinglePackageLocationData]
    }

customRepostoriesDataDecoder :: (Json.String -> [Json.String] -> e) -> D.Decoder e CustomRepositoriesData
customRepostoriesDataDecoder toErr = do
  customFullRepositories <- D.field "repositories" (D.list (customSingleRepositoryDataDecoder toErr))
  customSinglePackageRepositories <- D.field "single-package-locations" (D.list (singlePackageLocationDataDecoder toErr))
  pure $
    CustomRepositoriesData
      { _customFullRepositories=customFullRepositories
      , _customSinglePackageRepositories=customSinglePackageRepositories
      }