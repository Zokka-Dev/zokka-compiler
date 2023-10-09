module Deps.CustomRepositoryDataIO
  ( loadCustomRepositoriesData
  , CustomRepositoriesError
  )
  where

import Elm.CustomRepositoryData (CustomRepositoriesData, customRepostoriesDataDecoder)
import qualified Stuff
import qualified File
import qualified Json.Decode as D
import qualified Json.String as Json
import Data.Bifunctor (first)
import Stuff (ZelmCustomRepositoryConfigFilePath (..))

data CustomRepositoriesError = CRE_JsonDecodeError (D.Error IncorrectKeywordInJson)

data IncorrectKeywordInJson = IncorrectKeywordInJson
  { _expectedKeyword :: Json.String
  , _suggestions :: [Json.String]
  }

loadCustomRepositoriesData :: ZelmCustomRepositoryConfigFilePath -> IO (Either CustomRepositoriesError CustomRepositoriesData)
loadCustomRepositoriesData (ZelmCustomRepositoryConfigFilePath filePath) = do
  bytes <- File.readUtf8 filePath
  pure $ first CRE_JsonDecodeError (D.fromByteString (customRepostoriesDataDecoder IncorrectKeywordInJson) bytes)