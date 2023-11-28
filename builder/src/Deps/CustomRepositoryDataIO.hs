module Deps.CustomRepositoryDataIO
  ( loadCustomRepositoriesData
  , CustomRepositoriesError(..)
  , loadCustomRepositoriesDataForReactorTH
  )
  where

import Elm.CustomRepositoryData (CustomRepositoriesData, customRepostoriesDataDecoder, customRepostoriesDataEncoder, defaultCustomRepositoriesData, CustomRepositoryDataParseError, defaultCustomRepositoriesDataElmPackageRepoOnly)
import qualified File
import qualified Json.Decode as D
import qualified Json.Encode as E
import Data.Bifunctor (first)
import Stuff (ZelmCustomRepositoryConfigFilePath (..))

data CustomRepositoriesError = CREJsonDecodeError (D.Error CustomRepositoryDataParseError)
  deriving Show

-- FIXME: Boolean argument a hack for now
createCustomRepositoriesData :: ZelmCustomRepositoryConfigFilePath -> Bool -> IO (Either e CustomRepositoriesData)
createCustomRepositoriesData (ZelmCustomRepositoryConfigFilePath filePath) shouldIncludeZelmRepo = 
  let
    defaultData = if shouldIncludeZelmRepo then defaultCustomRepositoriesData else defaultCustomRepositoriesDataElmPackageRepoOnly
  in
  do
    E.write filePath (customRepostoriesDataEncoder defaultData)
    pure (Right defaultData)

loadCustomRepositoriesData :: ZelmCustomRepositoryConfigFilePath -> IO (Either CustomRepositoriesError CustomRepositoriesData)
loadCustomRepositoriesData z@(ZelmCustomRepositoryConfigFilePath filePath) = do
  customReposDataDoesExist <- File.exists filePath
  if customReposDataDoesExist
    then do
      bytes <- File.readUtf8 filePath
      pure $ first CREJsonDecodeError (D.fromByteString customRepostoriesDataDecoder bytes)
    else
      createCustomRepositoriesData z True

loadCustomRepositoriesDataForReactorTH :: ZelmCustomRepositoryConfigFilePath -> IO (Either CustomRepositoriesError CustomRepositoriesData)
loadCustomRepositoriesDataForReactorTH z@(ZelmCustomRepositoryConfigFilePath filePath) = do
  customReposDataDoesExist <- File.exists filePath
  if customReposDataDoesExist
    then do
      bytes <- File.readUtf8 filePath
      pure $ first CREJsonDecodeError (D.fromByteString customRepostoriesDataDecoder bytes)
    else
      createCustomRepositoriesData z False