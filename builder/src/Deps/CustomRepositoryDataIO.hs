module Deps.CustomRepositoryDataIO
  ( loadCustomRepositoriesData
  , CustomRepositoriesError
  )
  where

import Elm.CustomRepositoryData (CustomRepositoriesData, customRepostoriesDataDecoder, customRepostoriesDataEncoder, defaultCustomRepositoriesData)
import qualified Stuff
import qualified File
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String as Json
import Data.Bifunctor (first)
import Stuff (ZelmCustomRepositoryConfigFilePath (..))

data CustomRepositoriesError = CREJsonDecodeError (D.Error IncorrectKeywordInJson)

data IncorrectKeywordInJson = IncorrectKeywordInJson
  { _expectedKeyword :: Json.String
  , _suggestions :: [Json.String]
  }

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
      pure $ first CREJsonDecodeError (D.fromByteString (customRepostoriesDataDecoder IncorrectKeywordInJson) bytes)
    else
      createCustomRepositoriesData z