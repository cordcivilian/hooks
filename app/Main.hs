{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HM
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Word as Word
import qualified Data.Yaml as YAML
import qualified GHC.IO.Handle as Handle
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid
import qualified System.Directory as Dir
import qualified System.Environment as SysEnv
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.FilePath.Glob as Glob
import qualified System.IO as IO
import qualified System.IO.Error as IOError
import qualified System.Posix.Env as PosixEnv
import qualified System.Posix.IO as PosixIO
import qualified System.Process as Process

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

monolith :: IORef.IORef Config -> IORef.IORef Logger -> Wai.Application
monolith configRef loggerRef =
  Mid.logStdout $ \request respond -> do
    logger <- IORef.readIORef loggerRef
    body <- Wai.lazyRequestBody request
    let method = BSC.unpack $ Wai.requestMethod request
        path = BSC.unpack $ Wai.rawPathInfo request
        reqLogger = withContext logger "request" $ JSON.object
          [ "method" JSON..= method
          , "path" JSON..= path
          ]

    maybeSecret <- SysEnv.lookupEnv "HOOKER"
    Monad.when (Maybe.isNothing maybeSecret || method /= "GET") $
      logInfo reqLogger "http" $ T.pack $
        "Received " ++ method ++ " " ++ path

    response <- case (method, path) of
      ("GET", "/") -> return $ rootRoute request
      ("POST", "/events") ->
        hookRoute configRef reqLogger request body
      ("GET", "/hooked/list") -> do
        config <- IORef.readIORef configRef
        return $ hookedListRoute config
      ("GET", "/test") -> return $ testRoute request
      _ -> return notFoundRoute

    respond response

rootRoute :: Wai.Request -> Wai.Response
rootRoute request = Wai.responseLBS
  HTTP.status200
  [ (Headers.hContentType, BSC.pack "text/plain") ]
  ( BSLC.concat
    [ BSLC.fromStrict $ Wai.requestMethod request
    , BSLC.pack ": welcome to hooked!"
    ]
  )

hookRoute :: IORef.IORef Config -> Logger -> Wai.Request -> BSLC.ByteString
          -> IO Wai.Response
hookRoute configRef logger request body = do
  config <- IORef.readIORef configRef
  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  let secret = maybe "" id maybeSecret
      signature = lookup "x-hub-signature-256" $ Wai.requestHeaders request
      contextLogger = withContext logger "path"
        (JSON.String "/events")

  case (verifySignature body signature secret, getEventInfo body) of
    (Right (), Just event) -> case eventRef event of
      "refs/heads/main" -> do
        let fullName = eventRepoName event
            hookedRepos = remoteFullNameToHookedRepo config fullName
            eventLogger = withContext contextLogger "repo"
              (JSON.String $ T.pack fullName)

        mapM_ (syncRepo eventLogger config) hookedRepos

      _ -> return ()

    (Left err, _) ->
      logError contextLogger "webhook" $
        "Invalid signature: " <> err

    (_, Nothing) ->
      logError contextLogger "webhook" "Invalid event payload"

  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BSC.pack "text/plain")]
    (BSLC.pack "event processed")

hookedListRoute :: Config -> Wai.Response
hookedListRoute config = Wai.responseLBS
    HTTP.status200
    [ (Headers.hContentType, BSC.pack "text/plain") ]
    (BSLC.unlines $ map (BSLC.pack . show . repoPath) (repos config))

notFoundRoute :: Wai.Response
notFoundRoute = Wai.responseLBS
  HTTP.status404
  [(Headers.hContentType, BSC.pack "text/plain")]
  (BSLC.pack "404 - Not Found")

testRoute :: Wai.Request -> Wai.Response
testRoute request = Wai.responseLBS
  HTTP.status200
  [(Headers.hContentType, BSC.pack "text/plain")]
  (BSLC.pack $ show $ Wai.requestBodyLength request)

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

data LogLevel = Info | Warn | Error
  deriving (Show, Eq)

instance JSON.ToJSON LogLevel where
  toJSON Info = JSON.String "INFO"
  toJSON Warn = JSON.String "WARN"
  toJSON Error = JSON.String "ERROR"

data LogEntry = LogEntry
  { logTimestamp :: Clock.UTCTime
  , logLevel :: LogLevel
  , logMessage :: T.Text
  , logContext :: HM.HashMap T.Text JSON.Value
  , logType :: T.Text
  } deriving (Show)

instance JSON.ToJSON LogEntry where
  toJSON entry = JSON.object
    [ "timestamp" JSON..= formatISO8601 (logTimestamp entry)
    , "level" JSON..= logLevel entry
    , "message" JSON..= logMessage entry
    , "context" JSON..= logContext entry
    , "type" JSON..= logType entry
    ]

formatISO8601 :: Clock.UTCTime -> T.Text
formatISO8601 = T.pack . DateTimeFormat.formatTime
  DateTimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

data Logger = Logger
  { loggerWrite :: LogEntry -> IO ()
  , loggerContext :: HM.HashMap T.Text JSON.Value
  }

mkStdoutLogger :: IO Logger
mkStdoutLogger = do
  return Logger
    { loggerWrite = \entry -> TIO.putStrLn $
        TE.decodeUtf8 $ BSLC.toStrict $ JSON.encode entry
    , loggerContext = HM.empty
    }

logWith :: LogLevel -> Logger -> T.Text -> T.Text -> IO ()
logWith level logger type_ msg = do
  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  Monad.when (Maybe.isNothing maybeSecret || level /= Info) $ do
    timestamp <- Clock.getCurrentTime
    let entry = LogEntry
          { logTimestamp = timestamp
          , logLevel = level
          , logMessage = msg
          , logContext = loggerContext logger
          , logType = type_
          }
    loggerWrite logger entry

logInfo, logWarn, logError :: Logger -> T.Text -> T.Text -> IO ()
logInfo  = logWith Info
logWarn  = logWith Warn
logError = logWith Error

withContext :: Logger -> T.Text -> JSON.Value -> Logger
withContext logger key value = logger
  { loggerContext = HM.insert key value (loggerContext logger)
  }

createProcessPipes :: Logger -> FilePath -> IO (Handle.Handle, Handle.Handle)
createProcessPipes logger execPath = do
  (outReadFd, outWriteFd) <- PosixIO.createPipe
  (errReadFd, errWriteFd) <- PosixIO.createPipe

  outRead <- PosixIO.fdToHandle outReadFd
  outWrite <- PosixIO.fdToHandle outWriteFd
  errRead <- PosixIO.fdToHandle errReadFd
  errWrite <- PosixIO.fdToHandle errWriteFd

  let processLogger = withContext logger "process"
        (JSON.String $ T.pack execPath)

  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  let shouldLog = Maybe.isNothing maybeSecret

  _ <- Concurrent.forkIO $ Monad.forever $ do
    content <- IOError.tryIOError $ TIO.hGetLine outRead
    case content of
      Right line -> Monad.when shouldLog $
        logInfo processLogger "process_output" $ T.pack $
          execPath ++ " [stdout]: " ++ T.unpack line
      Left e | IOError.isEOFError e -> return ()
            | otherwise -> logError processLogger "process_output" $ T.pack $
                "Failed to read stdout: " ++ show e

  _ <- Concurrent.forkIO $ Monad.forever $ do
    content <- IOError.tryIOError $ TIO.hGetLine errRead
    case content of
      Right line -> logWarn processLogger "process_output" $ T.pack $
        execPath ++ " [stderr]: " ++ T.unpack line
      Left e | IOError.isEOFError e -> return ()
            | otherwise -> logError processLogger "process_output" $ T.pack $
                "Failed to read stderr: " ++ show e

  IO.hSetBuffering outWrite IO.LineBuffering
  IO.hSetBuffering errWrite IO.LineBuffering

  return (outWrite, errWrite)

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

data Event = Event
  { eventRef :: String
  , eventRepoName :: String
  } deriving (Show)

instance JSON.FromJSON Event where
  parseJSON = JSON.withObject "Event" $ \v -> Event
    <$> v JSON..: "ref"
    <*> (v JSON..: "repository" >>= (JSON..: "full_name"))

getEventInfo :: BSLC.ByteString -> Maybe Event
getEventInfo body =
    case JSON.eitherDecode body of
      Right event -> Just event
      _ -> Nothing

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

sign :: BSLC.ByteString -> String -> BSC.ByteString
sign body secret =
  let packedSecret = BSLC.pack secret
      hmacInstance = SHA.hmacSha256 packedSecret body
  in BSC.pack $ SHA.showDigest hmacInstance

verifySignature :: BSLC.ByteString
                -> Maybe BSC.ByteString
                -> String
                -> Either T.Text ()
verifySignature body signature secret = do
  case signature of
    Nothing -> Left "missing signature headers"
    Just digest -> do
      let
          packedSecret = BSLC.pack secret
          hmacInstance = SHA.hmacSha256 packedSecret body
          expected = BSC.pack $ SHA.showDigest $ hmacInstance
          actual = TE.encodeUtf8 $ T.drop 7 $ TE.decodeUtf8 digest
      if constantTimeCompare expected actual
          then Right ()
          else Left "signatures do not match"

constantTimeCompare :: BSC.ByteString -> BSC.ByteString -> Bool
constantTimeCompare a b =
  BSC.length a == BSC.length b &&
      0 == foldl'
          (\acc (x, y) -> acc Bits..|. Bits.xor x y)
          (0 :: Word.Word8)
          (BS.zip a b)

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

data BuildResult = BuildSuccess FilePath | BuildFailure String
  deriving (Show)

buildRepo :: Logger -> FilePath -> IO BuildResult
buildRepo logger path = do
  Dir.setCurrentDirectory path
  (exitCode, _, stderr) <- Process.readCreateProcessWithExitCode
    (Process.proc "cabal" ["build"]) ""
  case exitCode of
    Exit.ExitSuccess -> do
      (buildExit, buildPath, _) <- Process.readCreateProcessWithExitCode
        (Process.proc "cabal" ["list-bin", "."]) ""
      case buildExit of
        Exit.ExitSuccess ->
          case lines buildPath of
            [] -> do
              logError logger "build" "No executables found"
              return $ BuildFailure "No executables found"
            (exe:_) -> return $ BuildSuccess exe
        Exit.ExitFailure _ -> do
          logError logger "build" "Failed to locate built executable"
          return $ BuildFailure "Failed to locate built executable"
    Exit.ExitFailure _ -> do
      logError logger "build" $ T.pack $ "Build failed:\n" ++ stderr
      return $ BuildFailure $ "Build failed:\n" ++ stderr

handleBuildResult :: Logger -> Config -> Repo -> BuildResult -> Bool
                  -> IO (SyncResult ())
handleBuildResult logger config repo result shouldNotify =
  case result of
    BuildFailure err -> do
      logError logger "build" $ T.pack $ "Build failed: " ++ err
      return $ Left err
    BuildSuccess execPath -> do
      let deployDir = Path $ getReposDir config FP.</> "hooked-bin"
      deployDirResult <- ensureDeployDir logger (unPath deployDir)
      case deployDirResult of
        Left err -> do
          logError logger "build" $ T.pack $
            "Deploy directory setup failed: " ++ err
          return $ Left err
        Right () -> do
          deployResult <- deployExecutable logger execPath deployDir repo config
          case deployResult of
            Left err -> do
              logError logger "deploy" $ T.pack $
                "Deployment failed: " ++ err
              return $ Left err
            Right paths -> do
              logInfo logger "deploy" $ T.pack $
                "Successfully deployed: " ++ show paths
              if shouldNotify
                then notify logger (notifyUrl config) repo
                else return $ Right ()

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

ensureDeployDir :: Logger -> FilePath -> IO (SyncResult ())
ensureDeployDir logger deployDir = do
  result <- Exception.try $ Dir.createDirectoryIfMissing True deployDir
  case result of
    Right () -> return $ Right ()
    Left e -> do
      let err = "Failed to create deploy directory: " ++
                show (e :: Exception.SomeException)
      logError logger "deploy" $ T.pack err
      return $ Left err

deployExecutable :: Logger -> FilePath -> Path -> Repo -> Config
                -> IO (Either String [FilePath])
deployExecutable logger srcPath destDir repo config = do
  exists <- Dir.doesFileExist srcPath
  if exists
    then do
      let fileName = FP.takeFileName srcPath
          exeDir = unPath destDir FP.</> fileName
          destPath = exeDir FP.</> fileName
          backupPath = destPath ++ ".bak"

      Dir.createDirectoryIfMissing True exeDir

      destExists <- Dir.doesFileExist destPath
      Monad.when destExists $
        Dir.copyFile destPath backupPath

      maybeError <- Exception.tryJust
        (Just . Exception.displayException
            :: Exception.SomeException -> Maybe String)
        (Dir.copyFile srcPath destPath)
      case maybeError of
        Left err -> do
          Monad.when destExists $ do
            logWarn logger "deploy" "Deployment failed, restoring backup"
            Dir.copyFile backupPath destPath
          return $ Left err
        Right () -> do
          Dir.setPermissions destPath $
            Dir.setOwnerExecutable True $
            Dir.setOwnerReadable True $
            Dir.emptyPermissions

          case extraPaths repo of
            [] -> do
              logInfo logger "deploy" $ T.pack $
                "Deployed to " ++ destPath
              return $ Right [destPath]
            paths -> do
              let repoRoot = getReposDir config FP.</> unPath (repoPath repo)
              copyResult <- copyFilesAndDirs logger repoRoot exeDir
                             (map unPath paths)
              case copyResult of
                Left err -> return $ Left err
                Right copiedPaths -> do
                  logInfo logger "deploy" $ T.pack $
                    "Deployed executable and files to " ++ exeDir
                  return $ Right (destPath : copiedPaths)
    else do
      logError logger "deploy" "Source executable not found"
      return $ Left "Source executable not found"

copyFilesAndDirs :: Logger -> FilePath -> FilePath -> [FilePath]
                 -> IO (Either String [FilePath])
copyFilesAndDirs logger srcDir destDir paths = do
  let copyLogger = withContext logger "copy"
        (JSON.String $ T.pack $ "Copying files to " ++ destDir)

  Dir.createDirectoryIfMissing True destDir

  results <- Monad.forM paths $ \path -> do
    let srcPath = srcDir FP.</> path
        destPath = destDir FP.</> FP.takeFileName path

    exists <- Dir.doesPathExist srcPath
    if not exists
      then do
        let err = "Source path does not exist: " ++ srcPath
        logError copyLogger "copy" $ T.pack err
        return $ Left err
      else do
        isDir <- Dir.doesDirectoryExist srcPath
        if isDir
          then do
            contents <- Glob.glob $ srcPath FP.</> "**"
            Monad.forM_ contents $ \content -> do
              let relPath = FP.makeRelative srcPath content
                  newDestPath = destPath FP.</> relPath
              Dir.createDirectoryIfMissing True $ FP.takeDirectory newDestPath
              Dir.copyFile content newDestPath
            return $ Right destPath
          else do
            Dir.copyFile srcPath destPath
            return $ Right destPath

  case Either.partitionEithers results of
    ([], paths') -> return $ Right paths'
    (errs:_, _) -> return $ Left errs

deployAndNotify :: Logger -> Config -> Repo -> FilePath -> Path
                -> IO (SyncResult ())
deployAndNotify logger config repo execPath deployDir = do
  deployResult <- deployExecutable logger execPath deployDir repo config
  case deployResult of
    Left err -> do
      logError logger "deploy" $ T.pack $ "Deployment failed: " ++ err
      return $ Left err
    Right paths -> do
      logInfo logger "deploy" $ T.pack $
        "Successfully deployed: " ++ show paths
      notify logger (notifyUrl config) repo

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

runCommands :: Logger -> [(FilePath, [String])] -> IO Bool
runCommands _ [] = return True
runCommands logger ((cmd, args):rest) = do
  let procLogger = withContext logger "command"
        (JSON.String $ T.pack $ cmd ++ " " ++ unwords args)

  (_, Just stdout, Just stderr, ph) <- Process.createProcess (Process.proc cmd args)
    { Process.std_out = Process.CreatePipe
    , Process.std_err = Process.CreatePipe
    }

  exitCode <- Process.waitForProcess ph

  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  Monad.when (Maybe.isNothing maybeSecret) $ do
    outContent <- IO.hGetContents stdout
    Monad.unless (null outContent) $
      logInfo procLogger "command_output" $ T.pack outContent

  errContent <- IO.hGetContents stderr
  Monad.unless (null errContent) $
    logError procLogger "command_error" $ T.pack errContent

  case exitCode of
    Exit.ExitSuccess -> runCommands logger rest
    Exit.ExitFailure _ -> return False

initializeAllRepos :: Logger -> Config -> IO ()
initializeAllRepos logger config = do
  mapM_ (initRepo logger config) (repos config)
  where
    initRepo :: Logger -> Config -> Repo -> IO ()
    initRepo l c r = do
      result <- initializeRepo l c r
      case result of
        Left err ->
          logError l "init" $ T.pack $
            "Failed to initialize " ++
            unPath (repoPath r) ++ ": " ++ err
        Right () -> return ()

initializeRepo :: Logger -> Config -> Repo -> IO (SyncResult ())
initializeRepo logger config repo = do
  let baseDir = getReposDir config
      repoDir = FP.normalise $ baseDir FP.</> getLocalRepoDir repo

  exists <- Dir.doesDirectoryExist repoDir
  if exists
    then do
      fetchResult <- fetchLatestVersion logger repoDir
      case fetchResult of
        Right () -> do
          cleanResult <- Exception.try $
            runCommands logger [("cabal", ["clean"])]
          case cleanResult of
            Left e -> do
              logWarn logger "init" $ T.pack $
                "Failed to clean build directory (continuing anyway): " ++
                show (e :: Exception.SomeException)
              buildResult <- buildRepo logger repoDir
              handleBuildResult logger config repo buildResult False
            Right isSuccessful ->
              if isSuccessful
                then do
                  buildResult <- buildRepo logger repoDir
                  handleBuildResult logger config repo buildResult False
                else do
                  logWarn logger "init" $ T.pack $
                    "Failed to clean build directory (continuing anyway)"
                  buildResult <- buildRepo logger repoDir
                  handleBuildResult logger config repo buildResult False
        Left err -> do
          logError logger "init" $ T.pack $
            "Failed to fetch latest version: " ++ err
          return $ Left err
    else do
      Dir.createDirectoryIfMissing True baseDir
      isSuccessful <- runCommands logger
        [ ("git", [ "clone", "-q", getCloneURL repo, repoDir ]) ]

      case isSuccessful of
        True -> return $ Right ()
        False -> do
          let err = "Failed to clone repository"
          logError logger "init" $ T.pack err
          return $ Left err

fetchLatestVersion :: Logger -> FilePath -> IO (Either String ())
fetchLatestVersion logger path = do
  let gitLogger = withContext logger "repo_path"
        (JSON.String $ T.pack path)

  Dir.setCurrentDirectory path
  isSuccessful <- runCommands gitLogger
    [ ("git", ["fetch", "-q", "--atomic", "origin", "main"])
    , ("git", ["reset", "--hard", "origin/main"])
    , ("git", ["clean", "-dxqf"])
    ]

  case isSuccessful of
    True -> return $ Right ()
    False -> do
      logError gitLogger "fetch" "Git commands failed"
      return $ Left "Git commands failed"

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

type SyncResult = Either String

ensureRepoExists :: Logger -> FilePath -> IO (SyncResult ())
ensureRepoExists logger dir = do
  exists <- Dir.doesDirectoryExist dir
  if exists
    then return $ Right ()
    else do
      let err = "Repository directory not found: " ++ dir
      logError logger "check" $ T.pack err
      return $ Left err

syncRepo :: Logger -> Config -> Repo -> IO (SyncResult ())
syncRepo logger config repo = do
  let repoDir = FP.normalise $ getReposDir config FP.</> getLocalRepoDir repo
      repoLogger = withContext logger "repo_dir"
        (JSON.String $ T.pack repoDir)

  existsResult <- ensureRepoExists repoLogger repoDir
  case existsResult of
    Left err -> do
      logError repoLogger "sync" $ T.pack $
        "Repository check failed: " ++ err
      return $ Left err

    Right () -> do
      fetchResult <- fetchLatestVersion repoLogger repoDir
      case fetchResult of
        Left err -> do
          logError repoLogger "sync" $ T.pack $
            "Fetch failed: " ++ err
          return $ Left err

        Right () -> do
          cleanResult <- Exception.try $
            runCommands repoLogger [("cabal", ["clean"])]
          case cleanResult of
            Left e -> do
              logWarn repoLogger "sync" $ T.pack $
                "Failed to clean build directory (continuing anyway): " ++
                show (e :: Exception.SomeException)
              buildResult <- buildRepo repoLogger repoDir
              handleBuildResult repoLogger config repo buildResult True
            Right isSuccessful ->
              if not isSuccessful
                then do
                  logWarn repoLogger "sync" $ T.pack $
                    "Failed to clean build directory (continuing anyway)"
                  buildResult <- buildRepo repoLogger repoDir
                  handleBuildResult repoLogger config repo buildResult True
                else do
                  buildResult <- buildRepo repoLogger repoDir
                  handleBuildResult repoLogger config repo buildResult True

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

data Notification = Notification
  { sourceRepo :: String, sourceVersion :: String }
  deriving (Show)

instance JSON.ToJSON Notification where
  toJSON (Notification repo version) = JSON.object
      [ "repo" JSON..= repo
      , "version" JSON..= version
      ]

notify :: Logger -> URL -> Repo -> IO (Either String ())
notify logger notifyURL repo = do
  let notifyLogger = withContext logger "notify_url"
        (JSON.String $ T.pack $ unURL notifyURL)

  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  case maybeSecret of
    Nothing -> return $ Right ()
    Just secret -> do
      currentTime <- Clock.getCurrentTime
      initRequestResult <- Exception.try (HTTP.parseRequest $ unURL notifyURL)
      case initRequestResult of
        Left e -> do
          let errMsg = "Failed to parse URL: " ++
                          show (e :: Exception.SomeException)
          logError notifyLogger "notify" $ T.pack errMsg
          return $ Left errMsg

        Right initRequest -> do
          let hourlyVersion = ['a'..'z'] !!
                (read $ DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale "%H" currentTime)
              dailyVersion = DateTimeFormat.formatTime
                DateTimeFormat.defaultTimeLocale "%Y-%m-%d" currentTime
              body = JSON.encode $ Notification
                (getHtmlURL repo)
                (dailyVersion ++ [hourlyVersion])
              signature = [BSC.pack "sha256=", sign body secret]
              request = HTTP.setRequestMethod "POST" $
                HTTP.setRequestSecure True $
                HTTP.setRequestHeaders
                  [ (Headers.hContentType, BSC.pack "application/json")
                  , ( Headers.hContentLength
                    , BSC.pack $ show $ BSLC.length body
                    )
                  , ("Hooker-Signature-256", BSC.concat signature)
                  ] $
                HTTP.setRequestBodyLBS body initRequest

          responseResult <- Exception.try (HTTP.httpLBS request)
          case responseResult of
            Left e -> do
              let errMsg = "HTTP request failed: " ++
                    show (e :: HTTP.HttpException)
              logError notifyLogger "notify" $ T.pack errMsg
              return $ Left errMsg

            Right response -> case HTTP.getResponseStatusCode response of
              200 -> return $ Right ()
              code -> do
                let errMsg = "Invalid response code: " ++ show code
                logError notifyLogger "notify" $ T.pack errMsg
                return $ Left errMsg

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

type ProcessMap = [(FilePath, ProcessInfo)]

data ProcessInfo = ProcessInfo
  { procHandle :: Process.ProcessHandle
  , procExecPath :: FilePath
  , procStartTime :: Clock.UTCTime
  }

data ProcessEnv = ProcessEnv
  { processName :: String
  , envVars :: [(String, String)]
  } deriving (Show, Eq)

instance YAML.FromJSON ProcessEnv where
  parseJSON = YAML.withObject "ProcessEnv" $ \v -> ProcessEnv
    <$> v YAML..: "process"
    <*> v YAML..: "environment"

findExistingProcess :: FilePath -> IO (Maybe Int)
findExistingProcess execPath = do
  let cmd = Process.proc "pgrep" ["-f", execPath]
  result <- Exception.try $ Process.readCreateProcessWithExitCode cmd ""
  case result of
    Left e -> do
      TIO.putStrLn $ T.pack $ "Error checking process: " ++
        show (e :: Exception.SomeException)
      return Nothing
    Right (exitCode, stdout, _) -> case exitCode of
      Exit.ExitSuccess ->
        case lines stdout of
        [] -> return Nothing
        (firstLine:_) -> case reads firstLine of
          [(pid, "")] -> return $ Just pid
          _ -> return Nothing
      Exit.ExitFailure _ -> return Nothing

startNewProcess :: Logger -> IORef.IORef ProcessMap -> FilePath -> IO ()
startNewProcess logger procRef execPath = do
  existingPID <- findExistingProcess execPath
  case existingPID of
    Just pid -> do
      logInfo logger "process" $ T.pack $
        "Process already running with PID " ++ show pid
      currentTime <- Clock.getCurrentTime
      (_, _, _, handle) <- Process.createProcess (Process.proc "true" [])
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.NoStream
        , Process.std_err = Process.NoStream
        }
      let newProc = ProcessInfo
            { procHandle = handle
            , procExecPath = execPath
            , procStartTime = currentTime
            }
      IORef.modifyIORef procRef ((execPath, newProc):)
    Nothing -> do
      let binDir = FP.takeDirectory $ FP.takeDirectory execPath
          envFile = binDir FP.</> "process-env.yaml"
      envExists <- Dir.doesFileExist envFile
      mbEnv <- if not envExists
        then return Nothing
        else do
          result <- YAML.decodeFileEither envFile
            :: IO (Either YAML.ParseException [ProcessEnv])
          case result of
            Right envs -> do
              let matchingEnv = List.find
                    (\e -> processName e == FP.takeFileName execPath)
                    envs
              case matchingEnv of
                Just env -> return $ Just $ envVars env
                Nothing -> return Nothing
            Left err -> do
              logError logger "process" $ T.pack $
                "Failed to parse env file: " ++ show err
              return Nothing

      currentEnv <- PosixEnv.getEnvironment
      let processEnv = maybe currentEnv (++ currentEnv) mbEnv

      (outHandle, errHandle) <- createProcessPipes logger execPath

      (_, _, _, pHandle) <- Process.createProcess $ (Process.proc execPath [])
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.UseHandle outHandle
        , Process.std_err = Process.UseHandle errHandle
        , Process.env = Just processEnv
        }

      currentTime <- Clock.getCurrentTime
      let newProc = ProcessInfo
            { procHandle = pHandle
            , procExecPath = execPath
            , procStartTime = currentTime
            }
      procs <- IORef.readIORef procRef
      let updatedProcs = (execPath, newProc) :
                        filter ((execPath /=) . fst) procs
      IORef.writeIORef procRef updatedProcs
      logInfo logger "process" "Started new process"

ensureProcessRunning :: Logger -> IORef.IORef ProcessMap -> FilePath -> IO ()
ensureProcessRunning logger procRef execPath = do
  let procLogger = withContext logger "executable"
        (JSON.String $ T.pack execPath)

  existingPID <- findExistingProcess execPath
  case existingPID of
    Just pid -> do
      procs <- IORef.readIORef procRef
      case lookup execPath procs of
        Just _ -> return ()
        Nothing -> do
          currentTime <- Clock.getCurrentTime
          (_, _, _, handle) <- Process.createProcess (Process.proc "true" [])
            { Process.std_in = Process.NoStream
            , Process.std_out = Process.NoStream
            , Process.std_err = Process.NoStream
            }
          let newProc = ProcessInfo
                { procHandle = handle
                , procExecPath = execPath
                , procStartTime = currentTime
                }
          IORef.modifyIORef procRef ((execPath, newProc):)
          logInfo procLogger "process" $ T.pack $
            "Tracked existing process " ++ show pid

    Nothing -> do
      procs <- IORef.readIORef procRef
      case lookup execPath procs of
        Just info -> do
          mbExit <- Process.getProcessExitCode $ procHandle info
          case mbExit of
            Nothing -> return ()
            Just _ -> startNewProcess logger procRef execPath
        Nothing -> startNewProcess logger procRef execPath

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

mkPath :: FilePath -> Path
mkPath = Path . FP.normalise

mkURL :: String -> URLType -> Either String URL
mkURL url uType =
  case URI.parseURI url of
    Just uri | isValidScheme uri -> Right $ URL url uType
    Just _ -> Left "Invalid URL scheme: must be http(s)"
    Nothing -> Left "Invalid URL format"
  where
    isValidScheme uri = case URI.uriScheme uri of
      "http:" -> True
      "https:" -> True
      _ -> False

mkRepo :: FilePath -> URL -> [Path] -> Repo
mkRepo path url extra = Repo
  { repoPath = Path path
  , repoUrl = url
  , extraPaths = extra
  }

getReposDir :: Config -> FilePath
getReposDir = unPath . reposPath

getNotifyURL :: Config -> String
getNotifyURL = unURL . notifyUrl

getLocalRepoDir :: Repo -> FilePath
getLocalRepoDir = unPath . repoPath

getCloneURL :: Repo -> String
getCloneURL = unURL . repoUrl

getHtmlURL :: Repo -> String
getHtmlURL repo =
  let url = T.pack $ unURL $ repoUrl repo
  in case T.stripSuffix ".git" url of
       Just htmlURL -> T.unpack htmlURL
       Nothing -> T.unpack url

remoteFullNameToHookedRepo :: Config -> String -> [Repo]
remoteFullNameToHookedRepo config fullName =
  filter (isHookedOnRemote fullName) (repos config)

isHookedOnRemote :: String -> Repo -> Bool
isHookedOnRemote fullName repo =
  case URI.parseURI $ getHtmlURL repo of
    Just uri -> URI.uriPath uri == "/" ++ fullName
    Nothing -> False

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

newtype Path = Path
  { unPath :: FilePath
  } deriving (Eq, Show)

data URL = URL
  { unURL :: String
  , urlType :: URLType
  } deriving (Eq, Show)

data URLType = Clone
             | Notify deriving (Eq, Show)

data Config = Config
  { reposPath :: Path
  , notifyUrl :: URL
  , repos :: [Repo]
  } deriving (Eq, Show)

data Repo = Repo
  { repoPath :: Path
  , repoUrl :: URL
  , extraPaths :: [Path]
  } deriving (Eq, Show)

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

mkConfig :: FilePath -> Config
mkConfig dir = Config
  { reposPath = Path $ dir FP.</> "hooked-repos"
  , notifyUrl = URL "https://www.cordcivilian.com/updated" Notify
  , repos =
    [ mkRepo "cord"
        (URL "https://github.com/cordcivilian/cord.git" Clone)
        []
    , mkRepo "anorby"
        (URL "https://github.com/cordcivilian/anorby.git" Clone)
        [Path "data"]
    ]
  }

--DZJ-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

main :: IO ()
main = do
  mainLogger <- mkStdoutLogger
  loggerRef <- IORef.newIORef mainLogger
  let logger = mainLogger

  maybePort <- SysEnv.lookupEnv "PORT"
  let autoPort = 8888
      port = maybe autoPort read maybePort

  logInfo logger "startup" $ T.pack $
    "Server starting on port " ++ show (port :: Int)

  home <- Dir.getHomeDirectory
  let initialConfig = mkConfig home
  config <- IORef.newIORef initialConfig

  initializeAllRepos logger initialConfig

  procRef <- IORef.newIORef [] :: IO (IORef.IORef ProcessMap)
  let binDir = getReposDir initialConfig FP.</> "hooked-bin"
  Dir.createDirectoryIfMissing True binDir

  -- Build repositories
  let repos' = repos initialConfig
  mapM_
    (\repo -> do
      let repoLogger = withContext logger "repo"
            (JSON.String $ T.pack $ unPath $ repoPath repo)
      buildResult <- buildRepo repoLogger $
        FP.normalise $ getReposDir initialConfig FP.</>
          unPath (repoPath repo)
      Monad.void $
        handleBuildResult repoLogger initialConfig repo buildResult False
    ) repos'

  -- Start process monitor silently
  _ <- Concurrent.forkIO $ Monad.forever $ do
    let repoExecs = map (\repo ->
          let exeName = FP.takeFileName $ unPath $ repoPath repo
          in binDir FP.</> exeName FP.</> exeName) repos'
    mapM_
      (\execPath -> do
        exists <- Dir.doesFileExist execPath
        Monad.when exists $
          ensureProcessRunning logger procRef execPath
      ) repoExecs
    Concurrent.threadDelay 5000000

  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  Monad.when (Maybe.isNothing maybeSecret) $
    logInfo logger "startup" "Development server started"

  Warp.run port $ monolith config loggerRef
