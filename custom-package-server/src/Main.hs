{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), ToJSON, toJSON, object, Value(..))
import Web.Scotty
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text

data Package = Package 
    { pkgId :: Int
    , author :: Text
    , project :: Text
    , version :: Text
    , location :: Text
    , hash :: Text
    }
instance FromRow Package where
  fromRow = Package <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Package where
  toRow (Package pkgId author project version location hash) = toRow (pkgId, author, project, version, location, hash)
instance ToJSON Package where
  toJSON (Package pkgId author project version location hash) = object ["id" .= pkgId, "author" .= author, "project" .= project, "version" .= version, "location" .= location,  "hash" .= hash]

dbConfig :: String
dbConfig = "package_server_db.db"

getPackages :: IO [Package]
getPackages = do
  conn <- open dbConfig
  r <- query_ conn "SELECT * from Packages" :: IO [Package]
  close conn
  return r

getRecentPackages :: Int -> IO [Package]
getRecentPackages n = do
  conn <- open dbConfig
  r <- query conn "SELECT * from packages WHERE id > ?" (Only n)
  close conn
  return r

postPackage :: Package -> IO ()
postPackage pkg = do
  conn <- open dbConfig
  execute conn "INSERT INTO packages VALUES (?,?,?,?)" pkg
  close conn

main :: IO ()
main = scotty 3000 $ do

  post "/register" $ do
    name <- queryParam "name"
    version <- queryParam "version"
    let author : project : _ = splitOn "/" name
    liftIO $ postPackage (Package 0 author project version "some-location" "some-hash")
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
