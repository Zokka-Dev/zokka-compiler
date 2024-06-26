{-# LANGUAGE OverloadedStrings #-}
module Publish
  ( run
  , Args(..)
  )
  where


import Control.Exception (bracket_)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified Data.Utf8 as Utf8
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Info as Info
import qualified System.IO as IO
import qualified System.Process as Process

import qualified BackgroundWriter as BW
import qualified Build
import qualified Deps.Bump as Bump
import qualified Deps.Diff as Diff
import qualified Deps.Registry as Registry
import qualified Deps.Website as Website
import qualified Elm.Details as Details
import qualified Elm.Docs as Docs
import qualified Elm.Magnitude as M
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Json.String as Json
import qualified Reporting
import Reporting.Doc ((<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Task as Task
import qualified Stuff
import Elm.CustomRepositoryData (RepositoryUrl, RepositoryAuthToken, RepositoryLocalName, DefaultPackageServerRepo(..), PZRPackageServerRepo(..), CustomSingleRepositoryData (..))
import Deps.Website (standardElmPkgRepoDomain)
import Data.List (isInfixOf)
import Reporting.Exit (Publish(PublishWithNoRepositoryLocalName, PublishCustomRepositoryConfigDataError))
import Deps.CustomRepositoryDataIO (loadCustomRepositoriesData)
import Deps.Registry (createAuthHeader)
import qualified Codec.Archive.Zip as Zip
import Logging.Logger (printLog)



-- RUN

data Args
  = NoArgs
  | PublishToRepository RepositoryLocalName

-- TODO mandate no "exposing (..)" in packages to make
-- optimization to skip builds in Elm.Details always valid


run :: Args -> () -> IO ()
run args () =
  Reporting.attempt Exit.publishToReport $
    case args of
      NoArgs ->
        pure $ Left PublishWithNoRepositoryLocalName
      PublishToRepository repositoryUrl ->
        Task.run $
          do
            env <- getEnv
            publish env repositoryUrl



-- ENV


data Env =
  Env
    { _root :: FilePath
    , _cache :: Stuff.PackageCache
    , _manager :: Http.Manager
    , _registry :: Registry.ZokkaRegistries
    , _outline :: Outline.Outline
    }


getEnv :: Task.Task Exit.Publish Env
getEnv =
  do  root <- Task.mio Exit.PublishNoOutline $ Stuff.findRoot
      cache <- Task.io $ Stuff.getPackageCache
      zokkaCache <- Task.io $ Stuff.getZokkaCache
      manager <- Task.io $ Http.getManager
      reposConfigLocation <- Task.io $ Stuff.getOrCreateZokkaCustomRepositoryConfig
      customReposData <- Task.eio PublishCustomRepositoryConfigDataError $ loadCustomRepositoriesData reposConfigLocation
      zokkaRegistries <- Task.eio Exit.PublishMustHaveLatestRegistry $ Registry.latest manager customReposData zokkaCache reposConfigLocation
      outline <- Task.eio Exit.PublishBadOutline $ Outline.read root
      return $ Env root cache manager zokkaRegistries outline



-- PUBLISH

convertRegistryKeyToCustomSingleRepositoryData :: Registry.RegistryKey -> Maybe CustomSingleRepositoryData
convertRegistryKeyToCustomSingleRepositoryData registryKey =
  case registryKey of
    Registry.RepositoryUrlKey customSingleRepositoryData -> Just customSingleRepositoryData
    Registry.PackageUrlKey _ -> Nothing


allCustomSingleRepositoryDataFromRegistries :: Registry.ZokkaRegistries -> [CustomSingleRepositoryData]
allCustomSingleRepositoryDataFromRegistries Registry.ZokkaRegistries{Registry._registries=registriesMap} =
  Maybe.mapMaybe convertRegistryKeyToCustomSingleRepositoryData (Map.keys registriesMap)


localNameOfCustomSingleRepositoryData :: CustomSingleRepositoryData -> RepositoryLocalName
localNameOfCustomSingleRepositoryData customSingleRepositoryData =
  case customSingleRepositoryData of
    DefaultPackageServerRepoData defaultPackageServerRepo -> _defaultPackageServerRepoLocalName defaultPackageServerRepo
    PZRPackageServerRepoData pzrPackageServerRepo -> _pzrPackageServerRepoLocalName pzrPackageServerRepo


findKeyByLocalName :: RepositoryLocalName -> [CustomSingleRepositoryData] -> Maybe CustomSingleRepositoryData
findKeyByLocalName localName [] = Nothing
findKeyByLocalName localName (customSingleRepositoryData : restOfRepos) =
  if localNameOfCustomSingleRepositoryData customSingleRepositoryData == localName
    then Just customSingleRepositoryData
    else findKeyByLocalName localName restOfRepos


-- Consider whether a linear scan in a list of repositories is okay perf-wise
lookupRepositoryByLocalName :: RepositoryLocalName -> Registry.ZokkaRegistries -> Maybe CustomSingleRepositoryData
lookupRepositoryByLocalName repositoryLocalName zokkaRegistries =
  findKeyByLocalName repositoryLocalName (allCustomSingleRepositoryDataFromRegistries zokkaRegistries)


getAllLocalNamesInRegistries :: Registry.ZokkaRegistries -> [RepositoryLocalName]
getAllLocalNamesInRegistries registries = map localNameOfCustomSingleRepositoryData (allCustomSingleRepositoryDataFromRegistries registries)


-- The only relevant things we need to zip up are the elm.json file and all Elm
-- files in `src`. Note that even though elm.json for applications can specify
-- non-standard locations for source code, for packages, they must always be
-- under src.
--
-- We specifically do NOT pass a root argument and use purely relative paths
-- because absolute paths are actually quite annoying to deal with. In
-- particular, they potentially expose sensitive data about a user's filesystem.
--
-- So we instead expect always that we are in the correct location when this is
-- run (which is okay to do because the Elm compiler can only be run from the
-- top-level of a project when building).
--
-- If we ever want to make the location of the compiler configurable, we'll have
-- to revisit this.
createZipArchiveOfSourceCode :: IO Zip.Archive
createZipArchiveOfSourceCode = do
  elmFiles <- File.listAllElmFilesRecursively "src"
  -- Note that we must start with a "." entry. This is because the Elm compiler
  -- expects the very first entry of a ZIP archive to be the parent directory
  -- and then subtracts that prefix from every other file that is decompressed.
  -- If we use ".", this creates an entry with a zero-length file name (the
  -- period is effectively erased), which is what we want.
  --
  -- On the other hand if we used "", our underlying Zip library would blow up
  -- with an exception.
  let filesToZip = "." : "elm.json" : elmFiles
  printLog ("All the files we are zipping: " ++ show filesToZip)
  Zip.addFilesToArchive [] Zip.emptyArchive filesToZip


publish ::  Env -> RepositoryLocalName -> Task.Task Exit.Publish ()
publish env@(Env root _ manager registry outline) repositoryLocalName =
  case outline of
    Outline.App _ ->
      Task.throw Exit.PublishApplication

    Outline.Pkg (Outline.PkgOutline pkg summary _ vsn exposed _ _ _) ->
      case lookupRepositoryByLocalName repositoryLocalName registry of
        Nothing ->
          Task.throw (Exit.PublishUsingRepositoryLocalNameThatDoesntExistInCustomRepositoryConfig repositoryLocalName (getAllLocalNamesInRegistries registry))
        Just customRepositoriesData ->
          do  let maybeKnownVersions = Registry.getVersions pkg registry

              reportPublishStart pkg vsn maybeKnownVersions

              if noExposed  exposed then Task.throw Exit.PublishNoExposed else return ()
              if badSummary summary then Task.throw Exit.PublishNoSummary else return ()

              verifyReadme root
              verifyLicense root
              docs <- verifyBuild root
              verifyVersion env pkg vsn docs maybeKnownVersions

              Task.io $ putStrLn ""
              case customRepositoriesData of
                DefaultPackageServerRepoData defaultPackageServerRepo ->
                  let
                    repositoryUrl = _defaultPackageServerRepoTypeUrl defaultPackageServerRepo
                  in
                  if Utf8.toChars standardElmPkgRepoDomain `isInfixOf` Utf8.toChars repositoryUrl
                    then
                      Task.throw Exit.PublishToStandardElmRepositoryUsingZokka
                    else
                      do
                        git <- getGit
                        commitHash <- verifyTag git manager pkg vsn
                        verifyNoChanges git commitHash vsn
                        zipHash <- verifyZip env pkg vsn
                        register manager repositoryUrl pkg vsn docs commitHash zipHash
                PZRPackageServerRepoData pzrPackageServerRepo ->
                  do
                    Task.io $ putStrLn "Beginning to create in-memory ZIP archive of source code..."
                    zipArchive <- Task.io createZipArchiveOfSourceCode
                    Task.io $ putStrLn "Finished creating in-memory ZIP archive of source code!"
                    Task.io $ printLog ("All files in ZIP archive: " ++ show (Zip.filesInArchive zipArchive))
                    registerToPZRRepo
                      manager
                      (_pzrPackageServerRepoTypeUrl pzrPackageServerRepo) (_pzrPackageServerRepoAuthToken pzrPackageServerRepo)
                      pkg
                      vsn
                      docs
                      zipArchive
              Task.io $ putStrLn "Success!"


-- VERIFY SUMMARY


badSummary :: Json.String -> Bool
badSummary summary =
  Json.isEmpty summary || Outline.defaultSummary == summary


noExposed :: Outline.Exposed -> Bool
noExposed exposed =
  case exposed of
    Outline.ExposedList modules ->
      null modules

    Outline.ExposedDict chunks ->
      all (null . snd) chunks



-- VERIFY README


verifyReadme :: FilePath -> Task.Task Exit.Publish ()
verifyReadme root =
  reportReadmeCheck $
  do  let readmePath = root </> "README.md"
      exists <- File.exists readmePath
      case exists of
        False ->
          return (Left Exit.PublishNoReadme)

        True ->
          do  size <- IO.withFile readmePath IO.ReadMode IO.hFileSize
              if size < 300
                then return (Left Exit.PublishShortReadme)
                else return (Right ())



-- VERIFY LICENSE


verifyLicense :: FilePath -> Task.Task Exit.Publish ()
verifyLicense root =
  reportLicenseCheck $
  do  let licensePath = root </> "LICENSE"
      exists <- File.exists licensePath
      if exists
        then return (Right ())
        else return (Left Exit.PublishNoLicense)



-- VERIFY BUILD


verifyBuild :: FilePath -> Task.Task Exit.Publish Docs.Documentation
verifyBuild root =
  reportBuildCheck $ BW.withScope $ \scope ->
    Task.run $
    do  details@(Details.Details _ outline _ _ _ _) <-
          Task.eio Exit.PublishBadDetails $
            Details.load Reporting.silent scope root

        exposed <-
          case outline of
            Details.ValidApp _          -> Task.throw Exit.PublishApplication
            Details.ValidPkg _ []     _ -> Task.throw Exit.PublishNoExposed
            Details.ValidPkg _ (e:es) _ -> return (NE.List e es)

        Task.eio Exit.PublishBuildProblem $
          Build.fromExposed Reporting.silent root details Build.KeepDocs exposed


-- GET GIT


newtype Git =
  Git { _run :: [String] -> IO Exit.ExitCode }


getGit :: Task.Task Exit.Publish Git
getGit =
  do  maybeGit <- Task.io $ Dir.findExecutable "git"
      case maybeGit of
        Nothing ->
          Task.throw Exit.PublishNoGit

        Just git ->
          return $ Git $ \args ->
            let
              process =
                (Process.proc git args)
                  { Process.std_in  = Process.CreatePipe
                  , Process.std_out = Process.CreatePipe
                  , Process.std_err = Process.CreatePipe
                  }
            in
            Process.withCreateProcess process $ \_ _ _ handle ->
              Process.waitForProcess handle



-- VERIFY GITHUB TAG


verifyTag :: Git -> Http.Manager -> Pkg.Name -> V.Version -> Task.Task Exit.Publish String
verifyTag git manager pkg vsn =
  reportTagCheck vsn $
  do  -- https://stackoverflow.com/questions/1064499/how-to-list-all-git-tags
      exitCode <- _run git [ "show", "--name-only", V.toChars vsn, "--" ]
      case exitCode of
        Exit.ExitFailure _ ->
          return $ Left (Exit.PublishMissingTag vsn)

        Exit.ExitSuccess ->
          let url = toTagUrl pkg vsn in
          Http.get manager url [Http.accept "application/json"] (Exit.PublishCannotGetTag vsn) $ \body ->
            case D.fromByteString commitHashDecoder body of
              Right hash ->
                return $ Right hash

              Left _ ->
                return $ Left (Exit.PublishCannotGetTagData vsn url body)


toTagUrl :: Pkg.Name -> V.Version -> String
toTagUrl pkg vsn =
  "https://api.github.com/repos/" ++ Pkg.toUrl pkg ++ "/git/refs/tags/" ++ V.toChars vsn


commitHashDecoder :: D.Decoder e String
commitHashDecoder =
  Utf8.toChars <$>
    D.field "object" (D.field "sha" D.string)



-- VERIFY NO LOCAL CHANGES SINCE TAG


verifyNoChanges :: Git -> String -> V.Version -> Task.Task Exit.Publish ()
verifyNoChanges git commitHash vsn =
  reportLocalChangesCheck $
  do  -- https://stackoverflow.com/questions/3878624/how-do-i-programmatically-determine-if-there-are-uncommited-changes
      exitCode <- _run git [ "diff-index", "--quiet", commitHash, "--" ]
      case exitCode of
        Exit.ExitSuccess   -> return $ Right ()
        Exit.ExitFailure _ -> return $ Left (Exit.PublishLocalChanges vsn)



-- VERIFY THAT ZIP BUILDS / COMPUTE HASH


verifyZip :: Env -> Pkg.Name -> V.Version -> Task.Task Exit.Publish Http.Sha
verifyZip (Env root _ manager _ _) pkg vsn =
  withPrepublishDir root $ \prepublishDir ->
    do  let url = toZipUrl pkg vsn

        (sha, archive) <-
          reportDownloadCheck $
            Http.getArchive manager url
              Exit.PublishCannotGetZip
              (Exit.PublishCannotDecodeZip url)
              (return . Right)

        Task.io $ File.writePackage prepublishDir archive

        reportZipBuildCheck $
          Dir.withCurrentDirectory prepublishDir $
            verifyZipBuild prepublishDir

        return sha


toZipUrl :: Pkg.Name -> V.Version -> String
toZipUrl pkg vsn =
  "https://github.com/" ++ Pkg.toUrl pkg ++ "/zipball/" ++ V.toChars vsn ++ "/"


withPrepublishDir :: FilePath -> (FilePath -> Task.Task x a) -> Task.Task x a
withPrepublishDir root callback =
  let
    dir = Stuff.prepublishDir root
  in
  Task.eio id $
    bracket_
      (Dir.createDirectoryIfMissing True dir)
      (Dir.removeDirectoryRecursive dir)
      (Task.run (callback dir))


verifyZipBuild :: FilePath -> IO (Either Exit.Publish ())
verifyZipBuild root =
  BW.withScope $ \scope -> Task.run $
  do  details@(Details.Details _ outline _ _ _ _) <-
        Task.eio Exit.PublishZipBadDetails $
          Details.load Reporting.silent scope root

      exposed <-
        case outline of
          Details.ValidApp _          -> Task.throw Exit.PublishZipApplication
          Details.ValidPkg _ []     _ -> Task.throw Exit.PublishZipNoExposed
          Details.ValidPkg _ (e:es) _ -> return (NE.List e es)

      _ <- Task.eio Exit.PublishZipBuildProblem $
        Build.fromExposed Reporting.silent root details Build.KeepDocs exposed

      return ()



-- VERIFY VERSION


data GoodVersion
  = GoodStart
  | GoodBump V.Version M.Magnitude


verifyVersion :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe Registry.KnownVersions -> Task.Task Exit.Publish ()
verifyVersion env pkg vsn newDocs publishedVersions =
  reportSemverCheck vsn $
    case publishedVersions of
      Nothing ->
        if vsn == V.one
        then return $ Right GoodStart
        else return $ Left $ Exit.PublishNotInitialVersion vsn

      Just knownVersions@(Registry.KnownVersions latest previous) ->
        if vsn == latest || elem vsn previous
        then return $ Left $ Exit.PublishAlreadyPublished vsn
        else verifyBump env pkg vsn newDocs knownVersions


verifyBump :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Registry.KnownVersions -> IO (Either Exit.Publish GoodVersion)
verifyBump (Env _ cache manager registry _) pkg vsn newDocs knownVersions@(Registry.KnownVersions latest _) =
  case List.find (\(_ ,new, _) -> vsn == new) (Bump.getPossibilities knownVersions) of
    Nothing ->
      return $ Left $
        Exit.PublishInvalidBump vsn latest

    Just (old, new, magnitude) ->
      do  result <- Diff.getDocs cache registry manager pkg old
          case result of
            Left dp ->
              return $ Left $ Exit.PublishCannotGetDocs old new dp

            Right oldDocs ->
              let
                changes = Diff.diff oldDocs newDocs
                realNew = Diff.bump changes old
              in
              if new == realNew
              then return $ Right $ GoodBump old magnitude
              else
                return $ Left $
                  Exit.PublishBadBump old new magnitude realNew (Diff.toMagnitude changes)



-- REGISTER PACKAGES


register :: Http.Manager -> RepositoryUrl -> Pkg.Name -> V.Version -> Docs.Documentation -> String -> Http.Sha -> Task.Task Exit.Publish ()
register manager repositoryUrl pkg vsn docs commitHash sha =
  let
    url =
      Website.route repositoryUrl "/register"
        [ ("name", Pkg.toChars pkg)
        , ("version", V.toChars vsn)
        , ("commit-hash", commitHash)
        ]
  in
  Task.eio Exit.PublishCannotRegister $
    Http.upload manager url
      [ Http.filePart "elm.json" "elm.json"
      , Http.jsonPart "docs.json" "docs.json" (Docs.encode docs)
      , Http.filePart "README.md" "README.md"
      , Http.stringPart "github-hash" (Http.shaToChars sha)
      ]


registerToPZRRepo :: Http.Manager -> RepositoryUrl -> RepositoryAuthToken -> Pkg.Name -> V.Version -> Docs.Documentation -> Zip.Archive -> Task.Task Exit.Publish ()
registerToPZRRepo manager repositoryUrl repositoryAuthToken pkg vsn docs zipArchive =
  let
    url =
      Website.route repositoryUrl "/upload-package"
        [ ("name", Pkg.toChars pkg)
        , ("version", V.toChars vsn)
        ]
  in
  Task.eio Exit.PublishCannotRegister $
    Http.uploadWithHeaders manager url
      [ Http.filePart "elm.json" "elm.json"
      , Http.jsonPart "docs.json" "docs.json" (Docs.encode docs)
      , Http.filePart "README.md" "README.md"
      , Http.bytesPart "package.zip" "package.zip" (BS.toStrict $ Zip.fromArchive zipArchive)
      ]
      [ createAuthHeader repositoryAuthToken
      ]


-- REPORTING


reportPublishStart :: Pkg.Name -> V.Version -> Maybe Registry.KnownVersions -> Task.Task x ()
reportPublishStart pkg vsn maybeKnownVersions =
  Task.io $
  case maybeKnownVersions of
    Nothing ->
      putStrLn $ Exit.newPackageOverview ++ "\nI will now verify that everything is in order...\n"

    Just _ ->
      putStrLn $ "Verifying " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " ...\n"



-- REPORTING PHASES


reportReadmeCheck :: IO (Either x a) -> Task.Task x a
reportReadmeCheck =
  reportCheck
    "Looking for README.md"
    "Found README.md"
    "Problem with your README.md"


reportLicenseCheck :: IO (Either x a) -> Task.Task x a
reportLicenseCheck =
  reportCheck
    "Looking for LICENSE"
    "Found LICENSE"
    "Problem with your LICENSE"


reportBuildCheck :: IO (Either x a) -> Task.Task x a
reportBuildCheck =
  reportCheck
    "Verifying documentation..."
    "Verified documentation"
    "Problem with documentation"


reportSemverCheck :: V.Version -> IO (Either x GoodVersion) -> Task.Task x ()
reportSemverCheck version work =
  let
    vsn = V.toChars version

    waiting = "Checking semantic versioning rules. Is " ++ vsn ++ " correct?"
    failure = "Version " ++ vsn ++ " is not correct!"
    success result =
      case result of
        GoodStart ->
          "All packages start at version " ++ V.toChars V.one

        GoodBump oldVersion magnitude ->
          "Version number " ++ vsn ++ " verified ("
          ++ M.toChars magnitude ++ " change, "
          ++ V.toChars oldVersion ++ " => " ++ vsn ++ ")"
  in
  void $ reportCustomCheck waiting success failure work


reportTagCheck :: V.Version -> IO (Either x a) -> Task.Task x a
reportTagCheck vsn =
  reportCheck
    ("Is version " ++ V.toChars vsn ++ " tagged on GitHub?")
    ("Version " ++ V.toChars vsn ++ " is tagged on GitHub")
    ("Version " ++ V.toChars vsn ++ " is not tagged on GitHub!")


reportDownloadCheck :: IO (Either x a) -> Task.Task x a
reportDownloadCheck =
  reportCheck
    "Downloading code from GitHub..."
    "Code downloaded successfully from GitHub"
    "Could not download code from GitHub!"


reportLocalChangesCheck :: IO (Either x a) -> Task.Task x a
reportLocalChangesCheck =
  reportCheck
    "Checking for uncommitted changes..."
    "No uncommitted changes in local code"
    "Your local code is different than the code tagged on GitHub"


reportZipBuildCheck :: IO (Either x a) -> Task.Task x a
reportZipBuildCheck =
  reportCheck
    "Verifying downloaded code..."
    "Downloaded code compiles successfully"
    "Cannot compile downloaded code!"


reportCheck :: String -> String -> String -> IO (Either x a) -> Task.Task x a
reportCheck waiting success failure work =
  reportCustomCheck waiting (\_ -> success) failure work


reportCustomCheck :: String -> (a -> String) -> String -> IO (Either x a) -> Task.Task x a
reportCustomCheck waiting success failure work =
  let
    putFlush doc =
      Help.toStdout doc >> IO.hFlush IO.stdout

    padded message =
      message ++ replicate (length waiting - length message) ' '
  in
  Task.eio id $
  do  putFlush $ "  " <> waitingMark <+> D.fromChars waiting
      result <- work
      putFlush $
        case result of
          Right a -> "\r  " <> goodMark <+> D.fromChars (padded (success a) ++ "\n")
          Left _  -> "\r  " <> badMark  <+> D.fromChars (padded failure ++ "\n\n")

      return result


-- MARKS


goodMark :: D.Doc
goodMark =
  D.green $ if isWindows then "+" else "●"


badMark :: D.Doc
badMark =
  D.red $ if isWindows then "X" else "✗"


waitingMark :: D.Doc
waitingMark =
  D.dullyellow $ if isWindows then "-" else "→"


isWindows :: Bool
isWindows =
  Info.os == "mingw32"
