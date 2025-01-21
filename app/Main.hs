{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.IO as IO
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Word as Word
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid
import qualified System.Directory as Dir
import qualified System.Environment as SysEnv
import qualified System.Posix.Env as PosixEnv
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.Process as Process

data BuildResult = BuildSuccess FilePath | BuildFailure String
  deriving (Show)

type ProcessMap = [(FilePath, ProcessInfo)]

data ProcessInfo = ProcessInfo
  { procHandle :: Process.ProcessHandle
  , procExecPath :: FilePath
  , procStartTime :: Clock.UTCTime
  , procLogFile :: FilePath  -- New field for log file
  }

getProcessLogPath :: FilePath -> IO FilePath
getProcessLogPath execPath = do
  let logDir = FP.takeDirectory execPath FP.</> "logs"
  Dir.createDirectoryIfMissing True logDir
  let logFile = logDir FP.</> FP.takeBaseName execPath ++ ".log"
  return logFile

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

data ProcessEnv = ProcessEnv
  { processName :: String
  , envVars :: [(String, String)]
  } deriving (Show, Eq)

instance YAML.FromJSON ProcessEnv where
  parseJSON = YAML.withObject "ProcessEnv" $ \v -> ProcessEnv
    <$> v YAML..: "process"
    <*> v YAML..: "environment"

buildRepo :: Logger -> FilePath -> IO BuildResult
buildRepo logger repoPath = do
  logger $ T.pack $ "Building repo at " ++ repoPath
  Dir.setCurrentDirectory repoPath
  (exitCode, _, stderr) <- Process.readCreateProcessWithExitCode
    (Process.proc "cabal" ["build"]) ""
  case exitCode of
    Exit.ExitSuccess -> do
      (buildExit, buildPath, _) <- Process.readCreateProcessWithExitCode
        (Process.proc "cabal" ["list-bin", "."]) ""
      case buildExit of
        Exit.ExitSuccess ->
          case lines buildPath of
            [] -> return $ BuildFailure "No executables found"
            (exe:_) -> return $ BuildSuccess exe
        Exit.ExitFailure _ ->
          return $ BuildFailure "Failed to locate built executable"
    Exit.ExitFailure _ ->
      return $ BuildFailure $ "Build failed:\n" ++ stderr

deployExecutable :: Logger -> FilePath -> FilePath
                 -> IO (Either String FilePath)
deployExecutable logger srcPath destDir = do
  exists <- Dir.doesFileExist srcPath
  if exists
    then do
      let fileName = FP.takeFileName srcPath
          destPath = destDir FP.</> fileName
          backupPath = destPath ++ ".bak"
      destExists <- Dir.doesFileExist destPath
      Monad.when destExists $ do
        logger $ T.pack $ "Backing up existing executable to " ++ backupPath
        Dir.copyFile destPath backupPath
      logger $ T.pack $ "Deploying new executable to " ++ destPath
      maybeError <- Exception.tryJust
        (Just . Exception.displayException
            :: Exception.SomeException -> Maybe String)
        (Dir.copyFile srcPath destPath)
      case maybeError of
        Left err -> do
          Monad.when destExists $ do
            logger $ T.pack "Deployment failed, restoring backup"
            Dir.copyFile backupPath destPath
          return $ Left err
        Right () -> do
          Dir.setPermissions destPath $
            Dir.setOwnerExecutable True $
            Dir.setOwnerReadable True $
            Dir.emptyPermissions
          return $ Right destPath
    else return $ Left "Source executable not found"

ensureProcessRunning :: Logger -> IORef.IORef ProcessMap -> FilePath -> IO ()
ensureProcessRunning logger procRef execPath = do
  existingPID <- findExistingProcess execPath
  case existingPID of
    Just pid -> do
      -- Process already running, add to our map if not tracked
      procs <- IORef.readIORef procRef
      case lookup execPath procs of
        Just _ -> return ()  -- Already tracking this process
        Nothing -> do
          currentTime <- Clock.getCurrentTime
          (_, _, _, handle) <- Process.createProcess (Process.proc "true" [])
            { Process.std_in = Process.NoStream
            , Process.std_out = Process.NoStream
            , Process.std_err = Process.NoStream
            }
          logPath <- getProcessLogPath execPath
          let newProc = ProcessInfo
                { procHandle = handle
                , procExecPath = execPath
                , procStartTime = currentTime
                , procLogFile = logPath
                }
          IORef.modifyIORef procRef ((execPath, newProc):)
          logger $ T.pack $ "Found existing process for " ++ execPath ++
            " (PID: " ++ show pid ++ ", logs at " ++ logPath ++ ")"
    Nothing -> do
      procs <- IORef.readIORef procRef
      case lookup execPath procs of
        Just info -> do
          mbExit <- Process.getProcessExitCode $ procHandle info
          case mbExit of
            Nothing -> return ()  -- Process still running
            Just _ -> do
              logger $ T.pack $ "Process for " ++ execPath ++ " died, restarting"
              startNewProcess logger procRef execPath
        Nothing -> startNewProcess logger procRef execPath

startNewProcess :: Logger -> IORef.IORef ProcessMap -> FilePath -> IO ()
startNewProcess logger procRef execPath = do
  -- Double check no process exists right before starting
  existingPID <- findExistingProcess execPath
  case existingPID of
    Just pid -> do
      logger $ T.pack $ "Process already started by another instance " ++
        "(PID: " ++ show pid ++ ")"
      currentTime <- Clock.getCurrentTime
      (_, _, _, handle) <- Process.createProcess (Process.proc "true" [])
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.NoStream
        , Process.std_err = Process.NoStream
        }
      logPath <- getProcessLogPath execPath
      let newProc = ProcessInfo
            { procHandle = handle
            , procExecPath = execPath
            , procStartTime = currentTime
            , procLogFile = logPath
            }
      IORef.modifyIORef procRef ((execPath, newProc):)
    Nothing -> do
      let envFile = FP.takeDirectory execPath FP.</> "process-env.yaml"
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
                Just env -> do
                  logger $ T.pack $ "Loading environment for " ++ execPath
                  return $ Just $ envVars env
                Nothing -> return Nothing
            Left err -> do
              logger $ T.pack $ "Error parsing env file: " ++ show err
              return Nothing

      currentEnv <- PosixEnv.getEnvironment
      let processEnv = maybe currentEnv (++ currentEnv) mbEnv

      logPath <- getProcessLogPath execPath
      logHandle <- IO.openFile logPath IO.AppendMode
      IO.hSetBuffering logHandle IO.LineBuffering

      (_, _, _, pHandle) <- Process.createProcess $ (Process.proc execPath [])
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.UseHandle logHandle
        , Process.std_err = Process.UseHandle logHandle
        , Process.env = Just processEnv
        }

      currentTime <- Clock.getCurrentTime
      let newProc = ProcessInfo
            { procHandle = pHandle
            , procExecPath = execPath
            , procStartTime = currentTime
            , procLogFile = logPath
            }
      procs <- IORef.readIORef procRef
      let updatedProcs = (execPath, newProc) :
                        filter ((execPath /=) . fst) procs
      IORef.writeIORef procRef updatedProcs
      logger $ T.pack $ "Started new process for " ++ execPath ++
        " (logs at " ++ logPath ++ ")"

hooks :: IORef.IORef Config -> Logger -> Wai.Application
hooks statesRef logger request respond = do
  body <- Wai.lazyRequestBody request
  let request_method = BSC.unpack $ Wai.requestMethod request
      request_path = BSC.unpack $ Wai.rawPathInfo request
  response <- case (request_method, request_path) of
      ("GET", "/") -> return $ rootRoute request
      ("POST", "/events") -> do
          response <- hookRoute statesRef logger request body
          return response
      ("GET", "/hooked/list") -> do
          response <- hookedListRoute statesRef
          return response
      ("GET", "/test") -> return $ testRoute request
      _ -> return $ notFoundRoute
  respond response

type Logger = T.Text -> IO ()
monolith :: IORef.IORef Config -> Wai.Application
monolith statesRef = Mid.logStdout $ hooks statesRef TIO.putStrLn

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
hookRoute statesRef logger request body = do
  config <- IORef.readIORef statesRef
  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  let secret = maybe "" id maybeSecret
      signature = lookup "x-hub-signature-256" $ Wai.requestHeaders request
  case (verifySignature body signature secret, getEventInfo body) of
    (Right (), Just event) -> case ref event of
      "refs/heads/main" -> do
          let fullName = repoFullName event
              hookedRepo = remoteFullNameToHookedRepo config fullName
          mapM_ (syncRepo logger config) hookedRepo
          logger $ T.pack $ "syncing completed for " ++ fullName
      _ -> logger $ T.pack "not a main event"
    (_, _) -> logger $ T.pack "invalid event"
  return $ Wai.responseLBS
      HTTP.status200
      [(Headers.hContentType, BSC.pack "text/plain")]
      (BSLC.pack "event processed")

hookedListRoute :: IORef.IORef Config -> IO Wai.Response
hookedListRoute statesRef = do
  config <- IORef.readIORef statesRef
  return $ Wai.responseLBS
      HTTP.status200
      [ (Headers.hContentType, BSC.pack "text/plain") ]
      (BSLC.unlines $ map (BSLC.pack . show) (unRepos config))

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

-- ---------------------------------------------------------------------------

data Event = Event { ref :: String , repoFullName :: String } deriving (Show)

instance JSON.FromJSON Event where
  parseJSON = JSON.withObject "Event" $ \v -> Event
    <$> v JSON..: "ref"
    <*> (v JSON..: "repository" >>= (JSON..: "full_name"))

getEventInfo :: BSLC.ByteString -> Maybe Event
getEventInfo body =
    case JSON.eitherDecode body of
      Right event -> Just event
      _ -> Nothing

data Notification = Notification
  { sourceRepo :: String, sourceVersion :: String }
  deriving (Show)

instance JSON.ToJSON Notification where
  toJSON (Notification repo version) = JSON.object
      [ "repo" JSON..= repo
      , "version" JSON..= version
      ]

-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------

newtype AbsDir = AbsDir { unAbsDir :: String } deriving (Eq, Show)
newtype RelDir = RelDir { unRelDir :: String } deriving (Eq, Show)
newtype URL = URL { unURL :: String }  deriving (Eq, Show)

newtype ReposDir = ReposDir
  { unReposDir :: AbsDir } deriving (Eq, Show)
newtype LocalRepoDir = LocalRepoDir
  { unLocalRepoDir :: RelDir } deriving (Eq, Show)

newtype NotifyURL = NotifyURL { unNotifyURL :: URL } deriving (Eq, Show)
newtype CloneURL = CloneURL { unCloneURL :: URL } deriving (Eq, Show)

data Deploy = Deploy
  { reposDir :: ReposDir , notifyURL :: NotifyURL } deriving (Eq, Show)

data Repo = Repo
  { localRepoDir :: LocalRepoDir , cloneURL :: CloneURL } deriving (Eq, Show)

data Config = Config
  { unDeploy :: Deploy
  , unRepos :: [Repo]
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------

mkDeploy :: AbsDir -> URL -> Deploy
mkDeploy dir url = Deploy (ReposDir dir) (NotifyURL url)

mkRepo :: RelDir -> URL -> Repo
mkRepo dir url = Repo (LocalRepoDir dir) (CloneURL url)

getReposDir :: Config -> FilePath
getReposDir config = unAbsDir $ unReposDir $ reposDir $ unDeploy $ config

getNotifyURL :: Config -> String
getNotifyURL config = unURL $ unNotifyURL $ notifyURL $ unDeploy $ config

getLocalRepoDir :: Repo -> FilePath
getLocalRepoDir repo = unRelDir $ unLocalRepoDir $ localRepoDir $ repo

getCloneURL :: Repo -> String
getCloneURL repo = unURL $ unCloneURL $ cloneURL $ repo

getHtmlURL :: Repo -> String
getHtmlURL repo =
  let url = T.pack $ unURL $ unCloneURL $ cloneURL $ repo
  in case T.stripSuffix ".git" url of
       Just htmlURL -> T.unpack htmlURL
       Nothing -> T.unpack url

remoteFullNameToHookedRepo :: Config -> String -> [Repo]
remoteFullNameToHookedRepo config fullName =
  filter (isHookedOnRemote fullName) (unRepos config)

isHookedOnRemote :: String -> Repo -> Bool
isHookedOnRemote fullName repo =
  case URI.parseURI $ getHtmlURL repo of
    Just uri -> URI.uriPath uri == "/" ++ fullName
    Nothing -> False

-- ---------------------------------------------------------------------------

runCommands :: [(FilePath, [String])] -> IO Bool
runCommands [] = return True
runCommands (expr:rest) = do
  let cmd = fst expr
      args = snd expr
  (_, _, _, ph) <- Process.createProcess (Process.proc cmd args)
  exitCode <- Process.waitForProcess ph
  case exitCode of
      Exit.ExitSuccess -> runCommands rest
      Exit.ExitFailure _ -> return False

initializeAllRepos :: Logger -> Config -> IO ()
initializeAllRepos logger config = do
  let repos = unRepos config
  mapM_ (\repo -> do
    result <- initializeRepo logger config repo
    case result of
      Left err -> logger $ T.pack $
        "Failed to initialize " ++ getLocalRepoDir repo ++ ": " ++ err
      Right () -> logger $ T.pack $
        "Successfully initialized " ++ getLocalRepoDir repo
    ) repos

initializeRepo :: Logger -> Config -> Repo -> IO (SyncResult ())
initializeRepo logger config repo = do
  let baseDir = getReposDir config
      repoDir = FP.normalise $ baseDir FP.</> getLocalRepoDir repo

  exists <- Dir.doesDirectoryExist repoDir
  if exists
    then do
      logger $ T.pack $ "Fetching latest for " ++ getLocalRepoDir repo
      fetchResult <- fetchLatestVersion logger repoDir
      case fetchResult of
        Right () -> do
          notifyResult <- notify logger (getNotifyURL config) repo
          case notifyResult of
            Left err ->
              logger $ T.pack $ "Warning: Notification failed: " ++ err
            Right () -> return ()
          return $ Right ()
        Left err -> return $ Left err
    else do
      logger $ T.pack $ "Cloning " ++ getCloneURL repo
      Dir.createDirectoryIfMissing True baseDir
      isSuccessful <- runCommands
        [ ("git", [ "clone", "-q" , getCloneURL repo , repoDir ]) ]
      case isSuccessful of
        True -> do
          logger $ T.pack "Clone successful"
          return $ Right ()
        False -> do
          let err = "Failed to clone repository"
          logger $ T.pack err
          return $ Left err

fetchLatestVersion :: Logger -> FilePath -> IO (Either String ())
fetchLatestVersion logger repoPath = do
  logger $ T.pack "local repo found, fetching latest version"
  dirResult <- Exception.try (Dir.setCurrentDirectory repoPath)
  case dirResult of
    Left e -> return $ Left $
      "Failed to change directory: " ++ show (e :: Exception.SomeException)
    Right () -> do
      isSuccessful <- runCommands
        [ ("git", ["fetch", "-q", "--atomic", "origin", "main"])
        , ("git", ["reset", "--hard", "origin/main"])
        , ("git", ["clean", "-dxqf"])
        ]
      case isSuccessful of
        True -> return $ Right ()
        False -> return $ Left "Git commands failed"

notify :: Logger -> String -> Repo -> IO (Either String ())
notify logger url repo = do
  maybeSecret <- SysEnv.lookupEnv "HOOKER"
  case maybeSecret of
    Nothing -> do
      logger $ T.pack "Not in production, skipping notification"
      return $ Right ()
    Just secret -> do
      currentTime <- Clock.getCurrentTime
      initRequestResult <- Exception.try (HTTP.parseRequest url)
      case initRequestResult of
        Left e -> return $ Left $
          "Failed to parse URL: " ++ show (e :: Exception.SomeException)
        Right initRequest -> do
          let hourlyVersion = ['a'..'z'] !!
                (read $ DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale "%H" currentTime)
              dailyVersion = DateTimeFormat.formatTime
                DateTimeFormat.defaultTimeLocale "%Y-%m-%d" currentTime
              body = JSON.encode $
                Notification (getHtmlURL repo) (dailyVersion ++ [hourlyVersion])
              signature = [BSC.pack "sha256=", sign body secret]
              request = HTTP.setRequestMethod "POST" $
                HTTP.setRequestSecure True $
                HTTP.setRequestHeaders
                  [ (Headers.hContentType, BSC.pack "application/json")
                  , (Headers.hContentLength, BSC.pack $ show $ BSLC.length body)
                  , ("Hooker-Signature-256", BSC.concat signature)
                  ] $
                HTTP.setRequestBodyLBS body $
                initRequest
          responseResult <- Exception.try (HTTP.httpLBS request)
          case responseResult of
            Left e -> do
              let err = "HTTP request failed: " ++ show (e :: HTTP.HttpException)
              logger $ T.pack err
              return $ Left err
            Right response -> case HTTP.getResponseStatusCode response of
              200 -> do
                logger $ T.pack "hook event notified"
                return $ Right ()
              code -> do
                let err = "Invalid response code: " ++ show code
                logger $ T.pack err
                return $ Left err

type SyncResult = Either String

ensureRepoExists :: Logger -> FilePath -> IO (SyncResult ())
ensureRepoExists logger dir = do
  exists <- Dir.doesDirectoryExist dir
  if exists
    then return $ Right ()
    else do
      let err = "Repo dir " ++ dir ++ " does not exist"
      logger $ T.pack err
      return $ Left err

ensureDeployDir :: Logger -> FilePath -> IO (SyncResult ())
ensureDeployDir logger deployDir = do
  result <- Exception.try $ Dir.createDirectoryIfMissing True deployDir
  case result of
    Right () -> return $ Right ()
    Left e -> do
      let err = "Failed to create deploy directory: " ++
                show (e :: Exception.SomeException)
      logger $ T.pack err
      return $ Left err

deployAndNotify :: Logger -> Config -> Repo -> FilePath -> FilePath
                -> IO (SyncResult ())
deployAndNotify logger config repo execPath deployDir = do
  deployResult <- deployExecutable logger execPath deployDir
  case deployResult of
    Left err -> do
      logger $ T.pack $ "Deployment failed: " ++ err
      return $ Left err
    Right deployedPath -> do
      logger $ T.pack $ "Successfully deployed to " ++ deployedPath
      notifyResult <- notify logger (getNotifyURL config) repo
      case notifyResult of
        Left err -> do
          logger $ T.pack $ "Warning: Notification failed after deployment: " ++ err
          return $ Right () -- Continue despite notification failure
        Right () -> return $ Right ()

handleBuildResult :: Logger -> Config -> Repo -> BuildResult
                  -> IO (SyncResult ())
handleBuildResult logger config repo result =
  case result of
    BuildFailure err -> do
      logger $ T.pack $ "Build failed: " ++ err
      return $ Left err
    BuildSuccess execPath -> do
      let deployDir = getReposDir config FP.</> "hooked-bin"
      deployDirResult <- ensureDeployDir logger deployDir
      case deployDirResult of
        Left err -> return $ Left err
        Right () -> deployAndNotify logger config repo execPath deployDir

syncRepo :: Logger -> Config -> Repo -> IO (SyncResult ())
syncRepo logger config repo = do
  let repoDir = FP.normalise $ getReposDir config FP.</> getLocalRepoDir repo
  existsResult <- ensureRepoExists logger repoDir
  case existsResult of
    Left err -> return $ Left err
    Right () -> do
      fetchResult <- fetchLatestVersion logger repoDir
      case fetchResult of
        Left err -> do
          logger $ T.pack $ "Fetch failed: " ++ err
          return $ Left err
        Right () -> do
          notifyResult <- notify logger (getNotifyURL config) repo
          case notifyResult of
            Left err ->
              logger $ T.pack $ "Warning: Notification failed: " ++ err
            Right () -> return ()
          buildResult <- buildRepo logger repoDir
          handleBuildResult logger config repo buildResult

-- ---------------------------------------------------------------------------

mkConfig :: AbsDir -> Config
mkConfig dir = Config
  ( mkDeploy
      (AbsDir $ unAbsDir dir FP.</> "hooked-repos")
      (URL "https://www.cordcivilian.com/updated")
  )
  [ mkRepo (RelDir "cord")
           (URL "https://github.com/cordcivilian/cord.git")
  -- , mkRepo (RelDir "anorby")
  --          (URL "https://github.com/cordcivilian/anorby.git")
  ]

main :: IO ()
main = do
  maybePort <- SysEnv.lookupEnv "PORT"
  let autoPort = 8888
      port = maybe autoPort read maybePort
  putStrLn $ "Server starting on port " ++ show (port :: Int)

  home <- Dir.getHomeDirectory
  let initialConfig = mkConfig $ AbsDir home
  config <- IORef.newIORef initialConfig

  -- Initialize repositories
  initializeAllRepos TIO.putStrLn initialConfig

  procRef <- IORef.newIORef [] :: IO (IORef.IORef ProcessMap)
  let binDir = getReposDir initialConfig FP.</> "hooked-bin"
  Dir.createDirectoryIfMissing True binDir

  -- Build all repos initially
  let repos = unRepos initialConfig
  mapM_
    (\repo -> do
      buildResult <- buildRepo TIO.putStrLn $
        FP.normalise $ getReposDir initialConfig FP.</> getLocalRepoDir repo
      Monad.void $
        handleBuildResult TIO.putStrLn initialConfig repo buildResult
    ) repos

  -- Monitor processes
  _ <- Concurrent.forkIO $ Monad.forever $ do
    let repoExecs = map (binDir FP.</>) $ map getLocalRepoDir repos
    mapM_
      (\execPath -> do
        exists <- Dir.doesFileExist execPath
        Monad.when exists $
          ensureProcessRunning TIO.putStrLn procRef execPath
      ) repoExecs
    Concurrent.threadDelay 5000000  -- Check every 5 seconds

  Warp.run port $ monolith config
