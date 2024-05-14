{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Network.HTTP.Types.Status (status400, status401, Status, status403, status404)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), ToJSON, toJSON, object, Value(..), encode, decode)
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.Types as DAT
import Web.Scotty (scotty, get, post, pathParam, queryParam, json, status, files, File, ActionM, finish, status, setHeader, raw, header, middleware, formParam, Parsable, ScottyM)
import Database.SQLite.Simple (execute, execute_, query, query_, FromRow, ToRow, fromRow, toRow, Connection, field, withConnection, Only (..), withTransaction)
import Data.Text (Text, splitOn)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Text.Read (decimal)
import Data.String (fromString)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Network.Wai.Parse (FileInfo(..))
import Data.Maybe (maybe)
import qualified Data.Map as Map
import Web.Scotty.Trans.Strict (text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import qualified Database.SQLite.Simple.FromField as SSF
import qualified Database.SQLite.Simple.ToField as SST
import qualified Database.SQLite.Simple as SS
import qualified Database.SQLite.Simple.Ok as SSO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLBI
import Control.Monad (when)
import System.Entropy (getEntropy)
import qualified Crypto.Argon2 as CA
import qualified Data.ByteString.Base64 as Base64
import Data.Function ((&))
import qualified Data.Digest.Pure.SHA as SHA
import qualified Options.Applicative as OA
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (readFile)

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead _ = Nothing

unOnly :: Only a  -> a
unOnly (Only x) = x

newtype RepositoryId = RepositoryId { unRepositoryId :: Int }
  deriving (ToField, FromField, ToJSON, Parsable, Eq, Show)

newtype UserId = UserId { unUserId :: Int }
  deriving (ToField, FromField, ToJSON, Parsable, Eq, Show)

newtype Author = Author { unAuthor :: Text }
  deriving (ToField, FromField, ToJSON, Parsable, Eq, Show)

newtype Project = Project { unProject :: Text }
  deriving (ToField, FromField, ToJSON, Parsable, Eq, Show)

newtype Version = Version { unVersion :: Text }
  deriving (ToField, FromField, ToJSON, Parsable, Eq, Show)

newtype PackageHash = PackageHash { unPackageHash :: Text }
  deriving (ToField, FromField, ToJSON, Parsable, Eq, Show)

data Package = Package
  { pkgId :: Int
  , author :: Author
  , project :: Project
  , version :: Version
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
  , hash :: PackageHash
  , repositoryId :: RepositoryId
  , elmJson :: Value
  }
  deriving Show

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


serializePackageAsSingleString Package{author=author, project=project, version=version} = T.concat [unAuthor author, "/", unProject project, "@", unVersion version]


data AbbreviatedPackageInfo = AbbreviatedPackageInfo
  { _abbreviatedPackageInfoPkgKey :: Text
  , _abbreviatedPackageInfoVersions :: [Version]
  }
  deriving Show

abbreviatedPackageInfoToJsonFragment :: DAT.KeyValue e kv => AbbreviatedPackageInfo -> kv
abbreviatedPackageInfoToJsonFragment
  AbbreviatedPackageInfo
    {_abbreviatedPackageInfoPkgKey=pkgKey
    , _abbreviatedPackageInfoVersions=versions
    }
    = DAK.fromText pkgKey .= toJSON versions

packagesToOutgoingPackageData :: [Package] -> OutgoingPackageData
packagesToOutgoingPackageData packages =
  packages
    & fmap (\p -> (T.concat [unAuthor $ author p, "/", unProject $ project p], [version p]))
    & Map.fromListWith (++)
    & Map.toList
    & fmap (\(authorprojectkey, allversions) -> AbbreviatedPackageInfo{_abbreviatedPackageInfoPkgKey=authorprojectkey, _abbreviatedPackageInfoVersions=allversions})
    & OutgoingPackageData

data OutgoingPackageData = OutgoingPackageData [AbbreviatedPackageInfo]
  deriving Show

instance ToJSON OutgoingPackageData where
  toJSON (OutgoingPackageData abbreviatedPackageInfos) =
    object (fmap abbreviatedPackageInfoToJsonFragment abbreviatedPackageInfos)

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
  , partialPkgRepositoryId :: RepositoryId
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
  , pkgCreateReqRepositoryId :: RepositoryId
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

-- FIXME low: Maybe get rid of unsafePerformIO and manually thread this
-- everywhere. A bit tricky because we still want the bracketing behavior of
-- withConnection, which needs IO, which means we might have to do some annoying
-- IO <-> ActionM transformations if we want to just pass a highlevel Connection
-- everywhere.
-- This saves us the need to pass the db filename everywhere
dbFileName :: IORef String
{-# NOINLINE dbFileName #-}
dbFileName = unsafePerformIO (newIORef "package_server_db.db")

onDiskDbFileName :: String
onDiskDbFileName = "package_server_db.db"

inMemoryDbName :: String
inMemoryDbName = "file::memory:?cache=shared"

withCustomConnection :: (Connection -> IO a) -> IO a
withCustomConnection action =
  do
    currentDbName <- readIORef dbFileName
    withConnection currentDbName actionWithFKConstraints
  where
    actionWithFKConstraints conn = do
      execute_ conn "PRAGMA foreign_keys = ON"
      action conn


-- Maybe add? Be able to tell when a repository doesn't exist?
getPackages :: RepositoryId -> IO [Package]
getPackages repositoryId = withCustomConnection
  (\conn -> query conn "SELECT id, author, project, version, hash, repository_id, elm_json FROM packages p WHERE repository_id = ?" (Only repositoryId) :: IO [Package])

getRecentPackages :: Int -> RepositoryId -> IO [Package]
getRecentPackages n repositoryId = withCustomConnection
  (\conn -> query conn "SELECT id, author, project, version, hash, repository_id, elm_json FROM packages WHERE id > ? AND repository_id = ?" (n, repositoryId))

getPackageDataBlob :: RepositoryId -> Author -> Project -> Version -> Text -> IO (Maybe ByteString)
getPackageDataBlob repositoryId author project version filename = withCustomConnection
  (\conn -> fmap unOnly . safeHead <$> query conn "SELECT data FROM sqlar WHERE name = ?" lookupName)
  where
    lookupName = Only $ T.concat [intToText (unRepositoryId repositoryId) , "/" , unAuthor author , "/" , unProject project , "/" , unVersion version , "/" , filename]

getPackageDataBlobActionM :: RepositoryId -> Author -> Project -> Version -> Text -> ActionM ByteString
getPackageDataBlobActionM repositoryId author project version filename =
  do
    packageDataBlobMaybe <- liftIO $ getPackageDataBlob repositoryId author project version filename
    case packageDataBlobMaybe of
      Just packageDataBlob -> pure packageDataBlob
      Nothing ->
        do
          text $ T.concat ["Not able to find a blob for ", unAuthor author, "/", unProject project, "/", unVersion version, "/", filename]
          status status404
          finish

getElmJson :: RepositoryId -> Author -> Project -> Version -> IO (Maybe Value)
getElmJson repositoryId author project version = withCustomConnection
  (\conn -> fmap unOnly . safeHead <$> query conn "SELECT elm_json FROM packages WHERE repository_id = ? AND author = ? AND project = ? AND version = ?" (repositoryId, author, project, version))

getElmJsonActionM :: RepositoryId -> Author -> Project -> Version -> ActionM Value
getElmJsonActionM repositoryId author project version =
  do
    elmJsonMaybe <- liftIO $ getElmJson repositoryId author project version
    case elmJsonMaybe of
      Just elmJson -> pure elmJson
      Nothing ->
        do
          text $ T.concat ["Not able to find an elm.json file for ", unAuthor author, "/", unProject project, "@", unVersion version]
          status status404
          finish

generateEndpointJson :: Text -> RepositoryId -> Author -> Project -> Version -> IO (Maybe Value)
generateEndpointJson websitePrefix repositoryId author project version =
  do
    hashMaybe <- withCustomConnection queryForHash
    pure $ fmap hashToJsonObject hashMaybe
  where
    queryForHash :: Connection -> IO (Maybe Text)
    queryForHash conn = fmap unOnly . safeHead <$> query conn "SELECT hash FROM packages WHERE repository_id = ? AND author = ? AND project = ? AND version = ?" (repositoryId, author, project, version)

    hashToJsonObject hash = object
      [ "hash" .= hash
      , "url" .= T.concat [websitePrefix, "/", intToText (unRepositoryId repositoryId), "/packages/", unAuthor author, "/", unProject project, "/", unVersion version, "/package.zip"]
      ]

generateEndpointJsonActionM :: Text -> RepositoryId -> Author -> Project -> Version -> ActionM Value
generateEndpointJsonActionM websitePrefix repositoryId author project version =
  do
    endpointJsonMaybe <- liftIO $ generateEndpointJson websitePrefix repositoryId author project version
    case endpointJsonMaybe of
      Just endpointJson -> pure endpointJson
      Nothing ->
        do
          text $ T.concat ["Not able to find the package ", unAuthor author, "/", unProject project, "@", unVersion version]
          status status404
          finish

savePartialPackage :: Connection -> PartialPackage -> IO ()
savePartialPackage conn = execute conn "INSERT INTO packages (author, project, version, hash, repository_id, elm_json) VALUES (?,?,?,?,?,?)"

savePackageFiles :: Connection -> RepositoryId -> Text -> Text -> Text -> PackageFiles -> IO ()
savePackageFiles conn repositoryId author project version (PackageFiles docsJson readmeMd packageZip) =
  do
    saveAction "docs.json" (encode docsJson)
    saveAction "README.md" readmeMd
    saveAction "package.zip" packageZip
  where
    saveAction = saveSingleFile conn repositoryId author project version

saveNewPackage :: PackageCreationRequest -> IO ()
saveNewPackage pkg = withCustomConnection
  (\conn -> withTransaction conn $ do
    savePartialPackage conn partialPackage
    savePackageFiles
      conn
      (partialPkgRepositoryId partialPackage)
      (partialPkgAuthor partialPackage)
      (partialPkgProject partialPackage)
      (partialPkgVersion partialPackage)
      packageFiles
  )
  where
    (partialPackage, packageFiles) = splitPackageCreationRequest pkg

intToText :: Integral a => a -> T.Text
intToText = TL.toStrict . TLB.toLazyText . TLBI.decimal

saveSingleFile :: Connection -> RepositoryId -> Text -> Text -> Text-> Text -> ByteString -> IO ()
saveSingleFile conn repositoryId author project version fileName fileContent =
  execute conn "INSERT INTO sqlar (name, mode, mtime, sz, data) VALUES (?, 420, 0, length(?), ?)" (fileNameWithRepoId, fileContent, fileContent)
  where
    fileNameWithRepoId = T.concat [intToText (unRepositoryId repositoryId), "/", author, "/", project, "/", version, "/", fileName]

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
      text $ T.concat [ errorPrefix, decodeUtf8Lenient (BL.toStrict fileContent)]
      finish
    Just a ->
      pure a

parsePackageCreationRequest :: RepositoryId -> ActionM PackageCreationRequest
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
    -- We use SHA-1 because the Elm compiler uses SHA-1. At some point we'll
    -- probably want to switch away from it because SHA-1 is already showing
    -- signs of being worryingly weak.
    let sha1digest = T.pack (SHA.showDigest (SHA.sha1 packageZip))
    pure (PackageCreationRequest author project version sha1digest repositoryId elmJson docsJson readmeMd packageZip)

lastMaybe :: [a] -> Maybe a
lastMaybe xs = if not (null xs) then Just $ last xs else Nothing

mimeTypeFromFileName :: TL.Text -> TL.Text
mimeTypeFromFileName fileName =
  case lastMaybe (TL.splitOn "." fileName) of
    Just "md" -> "text/markdown"
    Just "json" -> "application/json"
    Just "zip" -> "application/zip"

customAuthSchemeName :: Text
customAuthSchemeName = "CustomZokkaRepoAuthToken"

newtype RepositoryAuthToken = RepositoryAuthToken { unRepositoryAuthToken :: Base64EncodedBytes }
-- Note that since we shouldn't be storing this token in the database, it should NOT have instances of ToField and FromField
  deriving (ToJSON, Parsable, Eq)

newtype RepositoryAuthTokenHash = RepositoryAuthTokenHash { unRepositoryAuthTokenHash :: ByteString }
  deriving (ToField, FromField, Eq)

deriveTokenFragment :: RepositoryAuthToken -> Text
deriveTokenFragment (RepositoryAuthToken (Base64EncodedBytes base64text)) = T.take 6 base64text

repositoryAuthTokenToText :: RepositoryAuthToken -> Text
repositoryAuthTokenToText (RepositoryAuthToken (Base64EncodedBytes base64text)) = base64text

-- Repository auth tokens tokens are randomly generated so an additional salt
-- gives us no real security advantage (since salts protect against rainbow
-- attacks which only make sense for duplicates, but we have enough random bits
-- that it is vanishingly unlikely that a duplicate token has ever been
-- generated)
repositoryAuthTokenSalt :: BS.ByteString
repositoryAuthTokenSalt = "aehirawehripawhrpawhprawhpoerhawperp"

hashRepositoryAuthToken :: RepositoryAuthToken -> RepositoryAuthTokenHash
hashRepositoryAuthToken (RepositoryAuthToken base64)= RepositoryAuthTokenHash (BS.fromStrict hashedBytes)
  where
    hashedBytes = securelyHashBytes (decodeBase64 base64) repositoryAuthTokenSalt

authTokenHashToRepositoryId :: RepositoryAuthTokenHash -> IO (Maybe (AuthTokenPermission, RepositoryId))
authTokenHashToRepositoryId authToken = withCustomConnection
  (\conn -> safeHead <$> query conn "SELECT permission_id, repository_id FROM auth_tokens WHERE token_value_hash = ?" (Only authToken))

authTokenToRepositoryId :: RepositoryAuthToken -> IO (Maybe (AuthTokenPermission, RepositoryId))
authTokenToRepositoryId authToken = authTokenHashToRepositoryId (hashRepositoryAuthToken authToken)

-- We use a Text instead of a ByteString as the underlying datatype because
-- ultimately we are using Base64 encoding when we have strings, which are not
-- arbitrary byte sequences.
newtype Base64EncodedBytes = Base64EncodedBytes { unBase64EncodedBytes :: Text }
  deriving (ToField, FromField, ToJSON, Parsable, Eq)

encodeBase64 :: BS.ByteString -> Base64EncodedBytes
encodeBase64 bytes = Base64EncodedBytes (decodeUtf8Lenient (Base64.encode bytes))

decodeBase64 :: Base64EncodedBytes -> BS.ByteString
decodeBase64 (Base64EncodedBytes base64EncodedBytes) =
  case Base64.decode (encodeUtf8 base64EncodedBytes) of
    Right bytes -> bytes
    -- We just error out because we should be guaranteed that a value of
    -- Base64EncodedBytes is in fact valid base64
    Left err -> error err

-- FIXME: Actually use the error that comes back from Base64.decode
validateTextIsBase64 :: Text -> Maybe Base64EncodedBytes
validateTextIsBase64 str =
  case Base64.decode (encodeUtf8 str) of
    Right _ -> Just $ Base64EncodedBytes str
    Left _ -> Nothing

newtype SessionToken = SessionToken { unSessionToken :: Base64EncodedBytes }
  deriving (ToField, FromField, ToJSON, Parsable, Eq)

newtype SessionTokenHash = SessionTokenHash { unSessionTokenHash :: ByteString }
  deriving (ToField, FromField, Eq)

-- Session tokens are randomly generated so an additional salt gives us no real
-- security advantage (since salts protect against rainbow attacks which only
-- make sense for duplicates, but we have enough random bits that it is
-- vanishingly unlikely that a duplicate token has ever been generated)
sessionTokenSalt :: BS.ByteString
sessionTokenSalt = "opawerpjawperjpawojrawjroawjrpajre"

hashSessionToken :: SessionToken -> SessionTokenHash
hashSessionToken (SessionToken sessionTokenRawText)= SessionTokenHash (BS.fromStrict hashedBytes)
  where
    hashedBytes = securelyHashBytes (decodeBase64 sessionTokenRawText) sessionTokenSalt

validateSessionTokenQuery :: Connection -> SessionTokenHash -> IO (Maybe UserId)
validateSessionTokenQuery conn sessionTokenHash =
  query conn "SELECT user_id FROM login_sessions WHERE session_token_value_hash = ?" (Only sessionTokenHash)
    & fmap (fmap unOnly . safeHead)

validateLoginSessionToken :: SessionTokenHash -> IO (Maybe UserId)
validateLoginSessionToken sessionToken = withCustomConnection (\conn -> validateSessionTokenQuery conn sessionToken)

sessionTokenToText :: SessionToken -> Text
sessionTokenToText (SessionToken sessionTokenAsBase64) = unBase64EncodedBytes sessionTokenAsBase64

retrieveRepositoryIdForAuthToken :: ActionM (AuthTokenPermission, RepositoryId)
retrieveRepositoryIdForAuthToken =
  do
    authTokenValue <- header "Authorization"
    case authTokenValue of
      Nothing ->
        do
          status status401
          text (T.concat ["WWW-Authenticate: ", customAuthSchemeName, " realm=\"Access to the repository API\""])
          finish
      Just authHeaderValue ->
        do
          -- FIXME: Have a more robust thing than just splitting on single
          -- space, should split more generally on whitespace
          let authScheme : authTokenText : _ = splitOn " " (TL.toStrict authHeaderValue)
          when (authScheme /= customAuthSchemeName) (do {status status400; text "Incorrect authentication scheme used!"; finish})
          repositoryAuthToken <- maybe (do { status status400; text "Token was not valid base 64!"; finish;}) (pure . RepositoryAuthToken) (validateTextIsBase64 authTokenText)
          repositoryIdMaybe <- liftIO $ authTokenToRepositoryId repositoryAuthToken
          case repositoryIdMaybe of
            Just (authPermission, repositoryId) -> pure (authPermission, repositoryId)
            Nothing ->
              do
                status status403
                text "Token not authorized for this repository"
                finish

retrieveUserIdForLoginSessionToken :: ActionM UserId
retrieveUserIdForLoginSessionToken =
  do
    authTokenValue <- header "Authorization"
    case authTokenValue of
      Nothing ->
        do
          status status401
          text (T.concat ["WWW-Authenticate: Basic realm=\"Access to the dashboard\""])
          finish
      Just authHeaderValue ->
        do
          -- FIXME: Have a more robust thing than just splitting on single
          -- space, should split more generally on whitespace
          -- FIXME: Deal with error cases here
          let authScheme : loginSessionToken : _ = splitOn " " (TL.toStrict authHeaderValue)
          -- FIXME: Deal with authScheme
          let sessionTokenMaybe = validateTextIsBase64 loginSessionToken
          sessionToken <- case sessionTokenMaybe of
            Nothing ->
              do
                status status400
                text "Login session token was not valid base64!"
                finish
            Just sessionTokenAsBase64 ->
              pure (SessionToken sessionTokenAsBase64)
          let sessionTokenHash = hashSessionToken sessionToken
          maybeUserId <- liftIO $ validateLoginSessionToken sessionTokenHash
          case maybeUserId of
            Nothing ->
              do
                status status403
                text "Invalid login session token!"
                finish
            Just userId ->
              pure userId

failOnCondition :: Text -> Status -> Bool -> ActionM ()
failOnCondition msg statusCode condition = when condition failureAction
  where
    failureAction =
      do
        status statusCode
        text msg
        finish

failOnWrongRepositoryId :: Bool -> ActionM ()
-- FIXME: Better error message
failOnWrongRepositoryId isWrongRepositoryId = failOnCondition "Wrong Repository ID!" status400 isWrongRepositoryId

failOnInsufficentPermissions :: Bool -> ActionM ()
failOnInsufficentPermissions isWrongRepositoryId = failOnCondition "Do not have sufficient permissions for your operation" status403 isWrongRepositoryId

authAgainstRepositoryId :: RepositoryId -> AuthTokenPermission -> ActionM ()
authAgainstRepositoryId expectedRepositoryId expectedPermission =
  do
    (permission, authedRepositoryId) <- retrieveRepositoryIdForAuthToken
    failOnWrongRepositoryId (expectedRepositoryId /= authedRepositoryId)
    failOnInsufficentPermissions (expectedPermission > permission)

saltSize :: Int
saltSize = 20


-- FIXME: Deal with this error case and see if it actually works
securelyHashBytes :: BS.ByteString -> BS.ByteString -> BS.ByteString
securelyHashBytes bytes salt = case CA.hash CA.defaultHashOptions bytes salt of
  Right successfulHash -> successfulHash
  -- FIXME: Deal with this error case (see if it's actually possible to hit this error route)
  Left err -> error (show err)

createUser :: Text -> Text -> IO ()
createUser username password =
  do
    salt <- getEntropy saltSize
    let passwordBytes = encodeUtf8 password
    let successfulHash = securelyHashBytes passwordBytes salt
    withCustomConnection
      (\conn -> execute conn "INSERT INTO users (username, password_hash, password_salt) VALUES (?,?,?)" (username, successfulHash, salt))


insertSessionTokenHashQuery :: Connection -> SessionTokenHash -> UserId -> IO ()
insertSessionTokenHashQuery conn sessionTokenHash userId = execute conn "INSERT INTO login_sessions (session_token_value_hash, user_id) VALUES (?, ?)" (sessionTokenHash, userId)

insertSessionTokenHash :: SessionTokenHash -> UserId -> IO ()
insertSessionTokenHash sessionTokenHash userId = withCustomConnection (\conn -> insertSessionTokenHashQuery conn sessionTokenHash userId)

sessionTokenSizeInBytes :: Int
sessionTokenSizeInBytes = 40

generateSessionToken :: IO SessionToken
generateSessionToken =
  do
    tokenAsBytes <- getEntropy sessionTokenSizeInBytes
    pure (SessionToken (encodeBase64 tokenAsBytes))

loginUser :: Text -> Text -> IO (Maybe SessionToken)
loginUser username password =
  do

    expectedPasswordHashAndPasswordSalt <- withCustomConnection
      (\conn -> safeHead <$> (query conn "SELECT password_hash, password_salt FROM users WHERE username = ?" (Only username) :: IO [(BS.ByteString, BS.ByteString)]))
    case expectedPasswordHashAndPasswordSalt of
      Nothing -> pure Nothing
      Just (expectedPasswordHash, passwordSalt) ->
        do
          let passwordBytes = encodeUtf8 password
          let passwordHashResult = securelyHashBytes passwordBytes passwordSalt
          newSessionToken <- generateSessionToken
          let newSessionTokenHash = hashSessionToken newSessionToken
          -- FIXME low: This is kind of weird not to do it in a single transaction, but there aren't any immediate issues
          userIdMaybe <- userIdByUserName username
          case userIdMaybe of
            Just userId ->
              do
                insertSessionTokenHash newSessionTokenHash userId
                if passwordHashResult == expectedPasswordHash
                  then pure (Just newSessionToken)
                  else pure Nothing
            Nothing ->
              pure Nothing

loginUserActionM :: Text -> Text -> ActionM SessionToken
loginUserActionM username password =
  do
    sessionTokenMaybe <- liftIO $ loginUser username password
    case sessionTokenMaybe of
      Just sessionToken -> pure sessionToken
      Nothing ->
        do
          text "Incorrect username or password!"
          status status403
          finish

-- FIXME: Actually do a JWT decode to find the user here
verifyLoginToken :: Text -> IO Text
verifyLoginToken = pure

userIdByUserNameQuery :: Connection -> Text -> IO (Maybe UserId)
userIdByUserNameQuery conn username = fmap unOnly . safeHead <$> query conn "SELECT id FROM users WHERE username = ?" (Only username)

userIdByUserName :: Text -> IO (Maybe UserId)
userIdByUserName username = withCustomConnection (\conn -> userIdByUserNameQuery conn username)

userHasAccessToRepositoryQuery :: Connection -> UserId -> RepositoryId -> IO (Only Bool)
userHasAccessToRepositoryQuery conn userId repositoryId =
  -- Note that this call to head is not unsafe because we are guaranteed that a COUNT call will always produce one row
  head <$> query conn "SELECT COUNT(id) > 0 FROM repositories WHERE owner_user_id = ? AND id = ?" (userId, repositoryId)

verifyUserIdHasAccessToRepository :: UserId -> RepositoryId -> IO Bool
verifyUserIdHasAccessToRepository userId repositoryId =
  withCustomConnection
    (\conn ->
      withTransaction conn $ do
        (Only hasAccess) <- userHasAccessToRepositoryQuery conn userId repositoryId
        pure hasAccess
    )

authUserIdAgainstRepository :: UserId -> RepositoryId -> ActionM ()
authUserIdAgainstRepository userId repositoryId =
  do
    hasAccess <- liftIO $ verifyUserIdHasAccessToRepository userId repositoryId
    failOnCondition (T.concat ["You do not have access to repository ", intToText (unRepositoryId repositoryId)]) status403 (not hasAccess)

tokenSizeInBytes :: Int
tokenSizeInBytes = 40

createTokenQuery :: Connection -> UserId -> RepositoryId -> AuthTokenPermission -> IO RepositoryAuthToken
createTokenQuery conn userId repositoryId permission =
  do
    tokenAsBytes <- getEntropy tokenSizeInBytes
    let token = RepositoryAuthToken $ encodeBase64 tokenAsBytes
    -- FIXME: Right now all tokens are created with read and write permissions
    execute conn "INSERT INTO auth_tokens (token_value_hash, token_value_fragment, user_id, repository_id, permission_id) VALUES (?,?,?,?,?)" (hashRepositoryAuthToken token, deriveTokenFragment token, userId, repositoryId, permission)
    pure token

createToken :: UserId -> RepositoryId -> AuthTokenPermission -> IO RepositoryAuthToken
createToken userId repositoryId permission = withCustomConnection
  (\conn -> withTransaction conn $
    createTokenQuery conn userId repositoryId permission
  )

createRepositoryQuery :: Connection -> Text -> Text -> UserId -> IO ()
createRepositoryQuery conn repositoryName repositorySafeUrlName userId =
  execute conn "INSERT INTO repositories (human_readable_name, url_safe_name, owner_user_id) VALUES (?,?,?)" (repositoryName, repositorySafeUrlName, userId)

searchRepositoryByRepositoryUrlSafeNAme :: Connection -> Text -> IO (Maybe RepositoryId)
searchRepositoryByRepositoryUrlSafeNAme conn repositorySafeUrlName =
  fmap unOnly . safeHead <$> query conn "SELECT id FROM repositories WHERE url_safe_name = ?" (Only repositorySafeUrlName)

createRepository :: Text -> Text -> UserId -> IO RepositoryId
createRepository repositoryName repositorySafeUrlName userId = withCustomConnection
  (\conn -> withTransaction conn $
    do
      createRepositoryQuery conn repositoryName repositorySafeUrlName userId
      repositoryIdMaybe <- searchRepositoryByRepositoryUrlSafeNAme conn repositorySafeUrlName
      case repositoryIdMaybe of
        Just repositoryId -> pure repositoryId
        Nothing -> error "This shouldn't be possible because we just created the repository we're reading from!"
  )

getTokensQuery :: Connection -> RepositoryId -> IO [Only Text]
getTokensQuery conn repositoryId = query conn "SELECT token_value_fragment FROM auth_tokens WHERE repository_id = ?" (Only repositoryId)

getTokens :: RepositoryId -> IO [Only Text]
getTokens repositoryId = withCustomConnection (\conn -> getTokensQuery conn repositoryId)

data AuthTokenPermission = ReadOnly | ReadWrite
  deriving Eq

instance ToField AuthTokenPermission where
  toField ReadOnly = SS.SQLInteger 0
  toField ReadWrite = SS.SQLInteger 1

instance FromField AuthTokenPermission where
  fromField :: SSF.FieldParser AuthTokenPermission
  fromField field = case SSF.fieldData field of
    SS.SQLBlob _ -> SSF.returnError SSF.ConversionFailed field "A permission ID is expected to be an integer (got data that was a blob)"
    SS.SQLFloat _ -> SSF.returnError SSF.ConversionFailed field "A permission ID is expected to be an integer (got data that was a float)"
    SS.SQLInteger x -> case x of
      0 -> SSO.Ok ReadOnly
      1 -> SSO.Ok ReadWrite
      other -> SSF.returnError SSF.ConversionFailed field ("Received an invalid permission ID (can only be 0 or 1): " ++ show other)
    SS.SQLText _ -> SSF.returnError SSF.ConversionFailed field "A permission ID is expected to be an integer (got data that was text)"
    SS.SQLNull -> SSF.returnError SSF.ConversionFailed field "A permission ID is expected to be an integer (got a null)"

instance Ord AuthTokenPermission where
  ReadOnly <= ReadWrite = True
  ReadWrite <= ReadOnly = False
  ReadOnly <= ReadOnly = True
  ReadWrite <= ReadWrite = True

instance ToJSON AuthTokenPermission where
  toJSON ReadOnly = "ReadOnly"
  toJSON ReadWrite = "ReadWrite"

data AuthToken = AuthToken
  { _authTokenValueFragment :: Text
  , _authTokenPermission :: AuthTokenPermission
  , _authTokenUserId :: Int
  }

instance ToJSON AuthToken where
  toJSON (AuthToken tokenValueFragment tokenPermission tokenUserId) = object
    [ "fragment" .= tokenValueFragment
    , "permission" .= tokenPermission
    , "userId" .= tokenUserId
    ]

data DashboardData = DashboardData
  { _dashboardDataPackages :: [Package]
  , _dashboardDataAuthTokens :: [AuthToken]
  }

instance ToJSON DashboardData where
  toJSON :: DashboardData -> Value
  toJSON (DashboardData packages authTokens) = object
    [ "packages" .= toJSON packages
    , "auth-tokens" .= toJSON authTokens
    ]

allPackagesForReposForUserIdQuery :: Connection -> UserId -> IO [Package]
allPackagesForReposForUserIdQuery conn userId =
  query conn "SELECT p.* FROM packages p INNER JOIN repositories r ON p.repository_id = r.id WHERE r.owner_user_id = ?" (Only userId)

allPackagesForReposForUserId :: UserId -> IO [Package]
allPackagesForReposForUserId userId = withCustomConnection (`allPackagesForReposForUserIdQuery` userId)

allAuthTokensForReposForUsernameQuery :: Connection -> UserId -> IO [AuthToken]
allAuthTokensForReposForUsernameQuery conn userId =
  fmap (fmap tupleToAuthToken) queryResult
  where
    queryResult :: IO [(Text, Int, Int)]
    queryResult = query conn "SELECT token_value_fragment, permission_id, user_id FROM auth_tokens WHERE user_id = ?" (Only userId)
    -- FIXME low: Deal with error case here (even those this is probably a db bug if it occurs)
    intToAuthTokenPermission 0 = ReadOnly
    intToAuthTokenPermission 1 = ReadWrite
    tupleToAuthToken (tokenValue, tokenPermission, userId) = AuthToken {_authTokenValueFragment=tokenValue, _authTokenPermission=intToAuthTokenPermission tokenPermission, _authTokenUserId=userId}

allAuthTokensForReposForUserId :: UserId -> IO [AuthToken]
allAuthTokensForReposForUserId userId = withCustomConnection (\conn -> allAuthTokensForReposForUsernameQuery conn userId)

getTopLevelDashboard :: UserId -> IO DashboardData
getTopLevelDashboard userId =
  do
    packages <- allPackagesForReposForUserId userId
    authTokens <- allAuthTokensForReposForUserId userId
    pure (DashboardData{_dashboardDataPackages=packages, _dashboardDataAuthTokens=authTokens})

scottyServer :: Text -> ScottyM ()
scottyServer externalWebsiteUrl =
  do
    middleware logStdout
    post "/:repository-id/upload-package" $ do
      -- Because we need the string :repository-id, I call pathParam here instead
      -- of moving pathParam into parsePackageCreationRequest to make sure that
      -- the strings match between pathParam and the string to post
      repositoryId <- pathParam "repository-id"
      authAgainstRepositoryId repositoryId ReadWrite
      packageCreationRequest <- parsePackageCreationRequest repositoryId
      liftIO $ saveNewPackage packageCreationRequest
      json $ object [ "success" .= String "Package registered successfully." ]

    get "/:repository-id/all-packages" $ do
      repositoryId <- pathParam "repository-id"
      authAgainstRepositoryId repositoryId ReadOnly
      packages <- liftIO $ getPackages repositoryId
      json (packagesToOutgoingPackageData packages)

    get "/:repository-id/all-packages/since/:n" $ do
      n <- pathParam "n"
      repositoryId <- pathParam "repository-id"
      authAgainstRepositoryId repositoryId ReadOnly
      packages <- liftIO $ getRecentPackages n repositoryId
      json (fmap serializePackageAsSingleString packages)

    -- Special case elm.json because it doesn't live in sqlar
    get "/:repository-id/packages/:author/:project/:version/elm.json" $ do
      repositoryId <- pathParam "repository-id"
      authAgainstRepositoryId repositoryId ReadOnly
      author <- pathParam "author"
      project <- pathParam "project"
      version <- pathParam "version"
      elmJson <- getElmJsonActionM repositoryId author project version
      json elmJson

    -- Special case endpoint.json because it's dynamically generated
    get "/:repository-id/packages/:author/:project/:version/endpoint.json" $ do
      repositoryId <- pathParam "repository-id"
      authAgainstRepositoryId repositoryId ReadOnly
      author <- pathParam "author"
      project <- pathParam "project"
      version <- pathParam "version"
      endpointJson <- generateEndpointJsonActionM externalWebsiteUrl repositoryId author project version
      json endpointJson

    get "/:repository-id/packages/:author/:project/:version/:filename" $ do
      repositoryId <- pathParam "repository-id"
      authAgainstRepositoryId repositoryId ReadOnly
      author <- pathParam "author"
      project <- pathParam "project"
      version <- pathParam "version"
      filename <- pathParam "filename"
      fileBlob <- getPackageDataBlobActionM repositoryId author project version filename
      setHeader "Content-Type" (mimeTypeFromFileName (TL.fromStrict filename))
      raw fileBlob

    post "/dashboard/user" $ do
      username <- formParam "username"
      password <- formParam "password"
      liftIO $ createUser username password
      text "Successfully created a user!"

    post "/dashboard/login" $ do
      username <- formParam "username"
      password <- formParam "password"
      loginToken <- loginUserActionM username password
      text (sessionTokenToText loginToken)

    post "/dashboard/repository" $ do
      userId <- retrieveUserIdForLoginSessionToken
      repositoryName <- queryParam "repository-name"
      repositoryUrlSafeName <- queryParam "repository-url-safe-name"
      repositoryId <- liftIO $ createRepository repositoryName repositoryUrlSafeName userId
      text (T.pack $ show $ unRepositoryId repositoryId)

    post "/dashboard/repository/:repository-id/token" $ do
      repositoryId <- pathParam "repository-id"
      userId <- retrieveUserIdForLoginSessionToken
      permissionLevelAsText <- queryParam "permission" :: ActionM Text
      permissionLevel <- case permissionLevelAsText of
        "readonly" -> pure ReadOnly
        "readwrite" -> pure ReadWrite
        x ->
          do
            status status400
            text (T.concat ["Gave an unknown permission level: ", x, ". Valid levels are readonly and readwrite"])
            finish
      tokenValue <- liftIO $ createToken userId repositoryId permissionLevel
      text (repositoryAuthTokenToText tokenValue)

    get "/dashboard/repository/:repository-id/all-tokens" $ do
      repositoryId <- pathParam "repository-id"
      userId <- retrieveUserIdForLoginSessionToken
      authUserIdAgainstRepository userId repositoryId
      tokens <- liftIO $ getTokens repositoryId
      let tokenStrings = fmap (\(Only x) -> x) tokens
      json tokenStrings

    get "/dashboard" $ do
      userId <- retrieveUserIdForLoginSessionToken
      dashboardData <- liftIO $ getTopLevelDashboard userId
      json dashboardData

    get "/" $ do
      text "Welcome to the custom package site!"

data CommandLineOptions = CommandLineOptions
  { _commandLineOptionsPortNumber :: Int
  -- FIXME: Use a Maybe instead of privileging the empty string
  , _commandLineOptionsSQLInitializationScriptLocation :: FilePath
  , _commandLineOptionsUseInMemoryDatabase :: Bool
  , _commandLineOptionsTurnOnTestMode :: Bool
  , _commandLineOptionsExternalWebsiteUrl :: Text
  }

commandLineArgParser :: OA.Parser CommandLineOptions
commandLineArgParser =
  CommandLineOptions
    <$> portNumber
    <*> initializationScriptLocation
    <*> useInMemoryDatabase
    <*> turnOnTestMode
    <*> externalWebsiteUrl
    where
      portNumber = OA.option OA.auto (OA.value 3000 <> OA.long "port" <> OA.short 'p' <> OA.metavar "PORT" <> OA.help "The port which the web server will bind to")
      initializationScriptLocation = OA.option OA.str (OA.value "" <> OA.long "initialization-script" <> OA.short 'i' <> OA.help "The location of the SQL initialization script")
      useInMemoryDatabase = OA.switch (OA.long "use-in-memory-database" <> OA.short 'u' <> OA.help "Use an in-memory SQLite database instead of storing to disk")
      turnOnTestMode = OA.switch (OA.long "test-mode" <> OA.short 't' <> OA.help "Run a short test script and then exit")
      externalWebsiteUrl = OA.option OA.str (OA.long "external-website-url" <> OA.short 'e' <> OA.help "The external website URL (necessary for packages to be read by the Zokka compiler). Make sure to leave the final slash off!")

commandLineArgParserInfo :: OA.ParserInfo CommandLineOptions
commandLineArgParserInfo = OA.info
  (commandLineArgParser OA.<**> OA.helper)
  (OA.fullDesc <> OA.progDesc "Custom package server for the Zokka compiler")

-- Function to split the script into individual statements
splitStatements :: String -> [String]
splitStatements script = filter (not . null) $ map strip $ splitOnSemicolon script

-- Helper function to split on semicolons
splitOnSemicolon :: String -> [String]
splitOnSemicolon script = case break (== ';') script of
    (before, "") -> [before]
    (before, _:after) -> before : splitOnSemicolon after

wschars :: String
wschars = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x wschars
                            then lstrip xs
                            else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse


-- Function to execute each SQL statement
executeStatements :: Connection -> [String] -> IO ()
executeStatements conn statements = withTransaction conn (action statements)
  where
    action = mapM_ (execute_ conn . SS.Query . fromString)

initializeDBFromFile :: FilePath -> IO ()
initializeDBFromFile filename =
  do
    fileContents <- readFile filename
    let fileContentsAsStatements = splitStatements fileContents
    print $ length fileContentsAsStatements
    print fileContentsAsStatements
    -- It's usually unsafe to directly put in a string instead of hardcoding a
    -- template, but we're relying on users to be submitting a single SQL file
    -- rather than opening this to the outside world
    withCustomConnection (\conn -> executeStatements conn fileContentsAsStatements)

main :: IO ()
main =
  do
    commandLineArgs <- OA.execParser commandLineArgParserInfo
    if _commandLineOptionsUseInMemoryDatabase commandLineArgs
      then
        do
          putStrLn "Using in-memory DB..."
          writeIORef dbFileName inMemoryDbName
      else
        do
          putStrLn "Using on-disk DB..."
          writeIORef dbFileName onDiskDbFileName
    let scriptLocation = _commandLineOptionsSQLInitializationScriptLocation commandLineArgs
    if scriptLocation == ""
      then pure ()
      else
        do
          putStrLn ("Initializing DB using " ++ scriptLocation)
          initializeDBFromFile scriptLocation
    scotty (_commandLineOptionsPortNumber commandLineArgs) (scottyServer (_commandLineOptionsExternalWebsiteUrl commandLineArgs))