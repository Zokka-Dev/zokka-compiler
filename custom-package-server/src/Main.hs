{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Network.HTTP.Types.Status (status400, status200)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), ToJSON, toJSON, object, Value(..), encode, decode)
import qualified Data.Aeson.Types as DAT
import Web.Scotty (scotty, get, post, pathParam, queryParam, json, status, files, File, ActionM, finish, status)
import Database.SQLite.Simple (execute, execute_, query, query_, FromRow, ToRow, fromRow, toRow, Connection, field, withConnection, Only (..), withTransaction)
import Data.Text (Text, splitOn)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Read (decimal)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Network.Wai.Parse (FileInfo(..))
import qualified Data.Map as Map
import Web.Scotty.Trans.Strict (text)
import qualified Data.ByteString.Lazy as BL
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import qualified Database.SQLite.Simple.FromField as SSF
import qualified Database.SQLite.Simple.ToField as SST
import qualified Database.SQLite.Simple as SS
import qualified Database.SQLite.Simple.Ok as SSO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLBI


data Package = Package
  { pkgId :: Int
  , author :: Text
  , project :: Text
  , version :: Text
  -- NOTE: Hash is actually kind of redundant. We only need a hash if we suspect
  -- that our storage layer for our packages.zip is unreliable, but in this case
  -- our storage layer is just our DB so if we suspect our DB, we should also
  -- suspect our hash.
  -- 
  -- But it's easier to store it at the moment because it reduces the amount of
  -- changes we need to make to the Zokka compiler, because we exactly replicate
  -- the Elm packages website's GET API and only need to customize the publish
  -- command. It also opens up possibilities in the future of moving package
  -- source code out of our SQLite database if that proves to be a performance
  -- bottleneck.
  , hash :: Text
  , repositoryId :: Int
  , elmJson :: Value
  }

instance FromRow Package where
  fromRow = Package <$> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Package where
  toRow (Package pkgId author project version hash repositoryId elmJson) = toRow (pkgId, author, project, version, hash, repositoryId, elmJson)
instance ToJSON Package where
  toJSON (Package pkgId author project version hash repositoryId elmJson) = object
    [ "id" .= pkgId
    , "author" .= author
    , "project" .= project
    , "version" .= version
    , "hash" .= hash
    , "repositoryId" .= repositoryId
    , "elmJson" .= elmJson
    ]

instance FromField Value where
  fromField :: SSF.FieldParser Value
  fromField field = case SSF.fieldData field of
    SS.SQLBlob byteString -> case decode (BL.fromStrict byteString) of 
      Nothing -> SSF.returnError SSF.ConversionFailed field "The SQL field was not valid JSON: "
      Just value -> SSO.Ok value
    SS.SQLFloat _ -> SSF.returnError SSF.ConversionFailed field "A JSON SQL field is expected to be a blob"
    SS.SQLInteger _ -> SSF.returnError SSF.ConversionFailed field "A JSON SQL field is expected to be a blob"
    SS.SQLText _ -> SSF.returnError SSF.ConversionFailed field "A JSON SQL field is expected to be a blob not a text field"
    SS.SQLNull -> SSO.Ok DAT.Null
instance ToField Value where
  toField = SS.SQLBlob . BL.toStrict . encode

data PartialPackage = PartialPackage
  { partialPkgAuthor :: Text
  , partialPkgProject :: Text
  , partialPkgVersion :: Text
  , partialPkgHash :: Text
  , partialPkgRepositoryId :: Int
  , partialPkgElmJson :: Value
  }
instance FromRow PartialPackage where
  fromRow = PartialPackage <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow PartialPackage where
  toRow (PartialPackage author project version hash repositoryId elmJson) =
    toRow (author, project, version, hash, repositoryId, elmJson)

data PackageFiles = PackageFiles
  { pkgFilesDocsJson :: Value
  , pkgFilesREADMEMd :: ByteString
  , pkgFilesPackageZip :: ByteString
  }
instance ToRow PackageFiles where
  toRow (PackageFiles docsJson readmeMd packageZip) = toRow (docsJson, readmeMd, packageZip)

data PackageCreationRequest = PackageCreationRequest
  { pkgCreateReqAuthor :: Text
  , pkgCreateReqProject :: Text
  , pkgCreateReqVersion :: Text
  , pkgCreateReqHash :: Text
  , pkgCreateReqRepositoryId :: Int
  , pkgCreateElmJson :: Value
  , pkgCreateDocsJson :: Value
  , pkgCreateREADMEMd :: ByteString
  , pkgCreatePackageZip :: ByteString
  }
instance FromRow PackageCreationRequest where
  fromRow = PackageCreationRequest <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow PackageCreationRequest where
  toRow (PackageCreationRequest author project version hash repositoryId elmJson docsJson readmeMd packageZip) =
    toRow (author, project, version, hash, repositoryId, elmJson, docsJson, readmeMd, packageZip)

splitPackageCreationRequest :: PackageCreationRequest -> (PartialPackage, PackageFiles)
splitPackageCreationRequest packageCreationRequest =
  (partialPackage, packageFiles)
  where
    PackageCreationRequest
      { pkgCreateReqAuthor=author
      , pkgCreateReqProject=project
      , pkgCreateReqVersion=version
      , pkgCreateReqHash=hash
      , pkgCreateReqRepositoryId=repositoryId
      , pkgCreateElmJson=elmJson
      , pkgCreateDocsJson=docsJson
      , pkgCreateREADMEMd=readmeMd
      , pkgCreatePackageZip=packageZip
      } = packageCreationRequest
    partialPackage = PartialPackage
      { partialPkgAuthor=author
      , partialPkgProject=project
      , partialPkgVersion=version
      , partialPkgHash=hash
      , partialPkgRepositoryId=repositoryId
      , partialPkgElmJson=elmJson
      }
    packageFiles = PackageFiles
      { pkgFilesDocsJson=docsJson
      , pkgFilesREADMEMd=readmeMd
      , pkgFilesPackageZip=packageZip
      }

dbConfig :: String
dbConfig = "package_server_db.db"

withCustomConnection :: String -> (Connection -> IO a) -> IO a
withCustomConnection dbName action = withConnection dbName actionWithFKConstraints
  where
    actionWithFKConstraints conn = do
      execute_ conn "PRAGMA foreign_keys = ON"
      action conn


getPackages :: IO [Package]
getPackages = withCustomConnection dbConfig
  (\conn -> query_ conn "SELECT * from packages" :: IO [Package])

getRecentPackages :: Int -> IO [Package]
getRecentPackages n = withCustomConnection dbConfig
  (\conn -> query conn "SELECT * from packages WHERE id > ?" (Only n))

savePartialPackage :: Connection -> PartialPackage -> IO ()
savePartialPackage conn = execute conn "INSERT INTO packages (author, project, version, hash, repository_id, elm_json) VALUES (?,?,?,?,?,?)"

savePackageFiles :: Connection -> Int -> Text -> Text -> PackageFiles -> IO ()
savePackageFiles conn repositoryId author project (PackageFiles docsJson readmeMd packageZip) =
  do
    saveAction "docs.json" (encode docsJson)
    saveAction "README.md" readmeMd
    saveAction "package.zip" packageZip
  where
    saveAction = saveSingleFile conn repositoryId author project

saveNewPackage :: PackageCreationRequest -> IO ()
saveNewPackage pkg = withCustomConnection dbConfig
  (\conn -> withTransaction conn $ do
    savePartialPackage conn partialPackage
    savePackageFiles 
      conn 
      (partialPkgRepositoryId partialPackage)
      (partialPkgAuthor partialPackage)
      (partialPkgProject partialPackage)
      packageFiles
  )
  where
    (partialPackage, packageFiles) = splitPackageCreationRequest pkg

intToText :: Integral a => a -> T.Text
intToText = TL.toStrict . TLB.toLazyText . TLBI.decimal

saveSingleFile :: Connection -> Int -> Text -> Text -> Text -> ByteString -> IO ()
saveSingleFile conn repositoryId author project fileName fileContent =
  execute conn "INSERT INTO sqlar (name, mode, mtime, sz, data) VALUES (?, 420, 0, length(?), ?)" (fileNameWithRepoId, fileContent, fileContent)
  where
    fileNameWithRepoId = T.concat [intToText repositoryId, "/", author, "/", project, "/", fileName]

filesToMap :: [File a] -> Map.Map Text (File a)
filesToMap = foldr insertFile Map.empty
  where
    -- FIXME: With fieldName use fileName instead
    insertFile file@(fieldName, _) = Map.insert fieldName file

decodeFormFileToJson :: FileInfo ByteString -> Maybe Value
decodeFormFileToJson FileInfo{fileContent=fileContent} = 
  decode fileContent

decodeFormFileToJsonActionM :: Text -> FileInfo ByteString -> ActionM Value
decodeFormFileToJsonActionM errorPrefix fileInfo@FileInfo{fileContent=fileContent} = 
  case decodeFormFileToJson fileInfo of
    Nothing -> do
      status status400
      text $ T.unwords [ errorPrefix,  decodeUtf8Lenient (BL.toStrict fileContent)]
      finish
    Just a ->
      pure a

parsePackageCreationRequest :: Int -> ActionM PackageCreationRequest
parsePackageCreationRequest repositoryId = do
    name <- queryParam "name" :: ActionM Text
    version <- queryParam "version" :: ActionM Text
    uploadedFiles <- files
    let filesMap = filesToMap uploadedFiles
    let author : project : _ = splitOn "/" name
    let (_, elmJsonFileInfo) = (Map.!) filesMap "elm.json"
    elmJson <- decodeFormFileToJsonActionM "You tried to upload an elm.json file but the file provided doesn't appear to be valid JSON. The file was: " elmJsonFileInfo
    let (_, docsJsonFileInfo) = (Map.!) filesMap "docs.json"
    docsJson <- decodeFormFileToJsonActionM "You tried to upload docs.json file but the file provided doesn't appear to be valid JSON. The file was: " docsJsonFileInfo
    let (_, FileInfo{fileContent=readmeMd}) = (Map.!) filesMap "README.md"
    let (_, FileInfo{fileContent=packageZip}) = (Map.!) filesMap "package.zip"
    pure (PackageCreationRequest author project version "some-hash" repositoryId elmJson docsJson readmeMd packageZip)

convertTextToRepositoryId :: Text -> ActionM Int
convertTextToRepositoryId repositoryIdText =
  case decimal repositoryIdText of
    Left errMsg -> do
      status status400
      -- FIXME: Make this error message better
      text $ T.unwords ["The repository ID provided was not a valid integer. It was: ", T.pack errMsg]
      finish
    Right (repositoryId, _) ->
      pure repositoryId

main :: IO ()
main = scotty 3000 $ do
  post "/:repository-id/upload-package" $ do
    -- Because we need the string :repository-id, I call pathParam here instead
    -- of moving pathParam into parsePackageCreationRequest to make sure that
    -- the strings match between pathParam and the string to post
    respositoryIdAsText <- pathParam "repository-id"
    repositoryId <- convertTextToRepositoryId respositoryIdAsText
    packageCreationRequest <- parsePackageCreationRequest repositoryId
    liftIO $ saveNewPackage packageCreationRequest
    status status200
    json $ object [ "success" .= String "Package registered successfully." ]

  get "/all-packages" $ do
    packages <- liftIO getPackages
    json packages

  get "/all-packages/since/:n" $ do
    n <- pathParam "n"
    packages <- liftIO $ getRecentPackages n
    json packages

  get "/" $ do
    text "Welcome to the custom package site!"
