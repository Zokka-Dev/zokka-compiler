{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.CustomRepositoryData
  ( CustomRepositoryData(..)
  , SinglePackageLocationData(..)
  , RepositoryType(..)
  , RepositoryUrl
  , PackageUrl
  , SinglePackageFileType(..)
  )
  where

import Elm.Version (Version)
import qualified Data.Utf8 as Utf8
import Elm.Package (Name)
import Network.HTTP (RequestMethod(Custom))
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String as Json
import qualified Data.Map as Map

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

decodeRepositoryType :: (Json.String -> [Json.String] -> e) -> D.Decoder e RepositoryType
decodeRepositoryType toError =
  do
    str <- D.string
    case lookupRepositoryType str of
      Right repositoryType -> pure repositoryType
      Left suggestions -> D.failure (toError str suggestions)

type RepositoryUrl = Utf8.Utf8 REPOSITORYURL
type PackageUrl = Utf8.Utf8 PACKAGEURL

data CustomRepositoryData =
  CustomRepositoryData
    { _repositoryType :: !RepositoryType
    , _repositoryUrl :: !RepositoryUrl
    }

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

singlePackageDecoder :: (Json.String -> [Json.String] -> e) -> D.Decoder e SinglePackageFileType
singlePackageDecoder toError =
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