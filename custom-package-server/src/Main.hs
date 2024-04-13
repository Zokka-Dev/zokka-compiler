{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Types.Status (status200)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), ToJSON, toJSON, object, Value(..))
import Web.Scotty (scotty, get, post, pathParam, queryParam, json, text, status, files, File, ActionM)
import Database.SQLite.Simple (execute, execute_, query, query_, FromRow, ToRow, fromRow, toRow, Connection, field, withConnection, Only (..))
import Data.Text (Text, splitOn)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Network.Wai.Parse (FileInfo(..))

data Package = Package 
  { pkgId :: Int
  , author :: Text
  , project :: Text
  , version :: Text
  , location :: Text
  , hash :: Text
  , repositoryId :: Int
  }

instance FromRow Package where
  fromRow = Package <$> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Package where
  toRow (Package pkgId author project version location hash repositoryId) = toRow (pkgId, author, project, version, location, hash, repositoryId)
instance ToJSON Package where
  toJSON (Package pkgId author project version location hash repositoryId) = object ["id" .= pkgId, "author" .= author, "project" .= project, "version" .= version, "location" .= location,  "hash" .= hash, "repositoryId" .= repositoryId]

data PackageCreationRequest = PackageCreationRequest
  { pkgCreateReqAuthor :: Text
  , pkgCreateReqProject :: Text
  , pkgCreateReqVersion :: Text
  , pkgCreateReqLocation :: Text
  , pkgCreateReqHash :: Text
  , pkgCreateReqRepositoryId :: Int
  }
instance FromRow PackageCreationRequest where
  fromRow = PackageCreationRequest <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow PackageCreationRequest where
  toRow (PackageCreationRequest author project version location hash repositoryId) = toRow (author, project, version, location, hash, repositoryId)


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

postPackage :: PackageCreationRequest -> IO ()
postPackage pkg = withCustomConnection dbConfig
  (\conn -> execute conn "INSERT INTO packages (author, project, version, location, hash, repository_id) VALUES (?,?,?,?,?,?)" pkg)

saveFile :: File ByteString -> IO ()
saveFile (fieldName, FileInfo{fileName=fileName, fileContent=fileContent}) =
  do
    withCustomConnection dbConfig
      (\conn -> execute conn "INSERT INTO sqlar (name, mode, mtime, sz, data) VALUES (?, 420, 0, length(?), ?)" (fileName, fileContent, fileContent))

main :: IO ()
main = scotty 3000 $ do
  post "/register" $ do
    name <- queryParam "name" :: ActionM Text
    version <- queryParam "version" :: ActionM Text
    uploadedFiles <- files
    liftIO $ print (length uploadedFiles)
    traverse_ (liftIO . saveFile) uploadedFiles
    let author : project : _ = splitOn "/" name
    liftIO $ postPackage (PackageCreationRequest author project version "some-location" "some-hash" 0)
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
