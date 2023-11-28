module Deps.CustomRepositoryDataIO
  ( loadCustomRepositoriesData
  , CustomRepositoriesError(..)
  )
  where

import Elm.CustomRepositoryData (CustomRepositoriesData, customRepostoriesDataDecoder, customRepostoriesDataEncoder, defaultCustomRepositoriesData, CustomRepositoryDataParseError)
import qualified File
import qualified Json.Decode as D
import qualified Json.Encode as E
import Data.Bifunctor (first)
import Stuff (ZelmCustomRepositoryConfigFilePath (..))

data CustomRepositoriesError = CREJsonDecodeError (D.Error CustomRepositoryDataParseError)
  deriving Show

createCustomRepositoriesData :: ZelmCustomRepositoryConfigFilePath -> IO (Either e CustomRepositoriesData)
createCustomRepositoriesData (ZelmCustomRepositoryConfigFilePath filePath) = do
  E.write filePath (customRepostoriesDataEncoder defaultCustomRepositoriesData)
  pure (Right defaultCustomRepositoriesData)

loadCustomRepositoriesData :: ZelmCustomRepositoryConfigFilePath -> IO (Either CustomRepositoriesError CustomRepositoriesData)
loadCustomRepositoriesData z@(ZelmCustomRepositoryConfigFilePath filePath) = do
  customReposDataDoesExist <- File.exists filePath
  if customReposDataDoesExist
    then do
      bytes <- File.readUtf8 filePath
      pure $ first CREJsonDecodeError (D.fromByteString customRepostoriesDataDecoder bytes)
    else
      createCustomRepositoriesData z