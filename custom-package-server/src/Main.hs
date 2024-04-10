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

data Package = Package 
    { pkgId :: String
    , name :: String
    , version :: String
    , location :: String
    , hash :: String
    }
instance FromRow Package where
  fromRow = Package <$> field <*> field <*> field <*> field <*> field
instance ToRow Package where
  toRow (Package pkgId name version location hash) = toRow (pkgId, name, version, location, hash)
instance ToJSON Package where
  toJSON (Package pkgId name version location hash) = object ["id" .= pkgId, "name" .= name, "version" .= version, "location" .= location,  "hash" .= hash]

dbConfig :: String
dbConfig = "dbname=package_server.db"

getPackages :: IO [Package]
getPackages = do
  conn <- open dbConfig
  r <- query_ conn "SELECT * from packages" :: IO [Package]
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
    name <- param "name"
    version <- param "version"
    location <- param "location"
    hash <- param "hash"
    liftIO $ postPackage (Package name version location hash)
    status status200
    json $ object [ "success" .= String "Package registered successfully." ]

  get "/all-packages" $ do
    packages <- liftIO getPackages
    json packages

  get "/all-packages/since/:n" $ do
    n <- param "n"
    packages <- liftIO $ getRecentPackages n
    json packages

  get "/" $ do
    text "Welcome to the custom package site!"
