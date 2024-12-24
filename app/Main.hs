{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment as Env
import qualified System.Process as Process
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Bits as Bits
import qualified Data.Word as Word
import qualified Data.IORef as IOR
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as DateTimeFormat

import qualified Network.URI as URI
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

hooks :: IOR.IORef Config -> Logger -> Wai.Application
hooks statesRef logger request respond = do
    body <- Wai.lazyRequestBody request
    let request_method = BS.unpack $ Wai.requestMethod request
        request_path = BS.unpack $ Wai.rawPathInfo request
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
monolith :: IOR.IORef Config -> Wai.Application
monolith statesRef = Mid.logStdout $ hooks statesRef TIO.putStrLn

rootRoute :: Wai.Request -> Wai.Response
rootRoute request = Wai.responseLBS
    HTTP.status200
    [ (Headers.hContentType, BS.pack "text/plain") ]
    ( BSL.concat
        [ BSL.fromStrict $ Wai.requestMethod request
        , BSL.pack ": welcome to hooked!"
        ]
    )

hookRoute :: IOR.IORef Config
          -> Logger
          -> Wai.Request
          -> BSL.ByteString
          -> IO Wai.Response
hookRoute statesRef logger request body = do
    config <- IOR.readIORef statesRef
    maybeSecret <- Env.lookupEnv "HOOKER"
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
        [(Headers.hContentType, BS.pack "text/plain")]
        (BSL.pack "event processed")

hookedListRoute :: IOR.IORef Config -> IO Wai.Response
hookedListRoute statesRef = do
    config <- IOR.readIORef statesRef
    return $ Wai.responseLBS
        HTTP.status200
        [ (Headers.hContentType, BS.pack "text/plain") ]
        (BSL.unlines $ map (BSL.pack . show) (unRepos config))

notFoundRoute :: Wai.Response
notFoundRoute = Wai.responseLBS
    HTTP.status404
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack "404 - Not Found")

testRoute :: Wai.Request -> Wai.Response
testRoute request = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack $ show $ Wai.requestBodyLength request)

-- ---------------------------------------------------------------------------

data Event = Event { ref :: String , repoFullName :: String } deriving (Show)
instance JSON.FromJSON Event where
    parseJSON = JSON.withObject "Event" $ \v -> Event
        <$> v JSON..: "ref"
        <*> (v JSON..: "repository" >>= (JSON..: "full_name"))

getEventInfo :: BSL.ByteString -> Maybe Event
getEventInfo body = case JSON.eitherDecode body of
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

sign :: BSL.ByteString -> String -> BS.ByteString
sign body secret =
    let packedSecret = BSL.pack secret
        hmacInstance = SHA.hmacSha256 packedSecret body
    in BS.pack $ SHA.showDigest hmacInstance

verifySignature :: BSL.ByteString
                -> Maybe BS.ByteString
                -> String
                -> Either T.Text ()
verifySignature body signature secret = do
    case signature of
        Nothing -> Left "missing signature headers"
        Just digest -> do
            let
                packedSecret = BSL.pack secret
                hmacInstance = SHA.hmacSha256 packedSecret body
                expected = BS.pack $ SHA.showDigest $ hmacInstance
                actual = TE.encodeUtf8 $ T.drop 7 $ TE.decodeUtf8 digest
            if constantTimeCompare expected actual
                then Right ()
                else Left "signatures do not match"

constantTimeCompare :: BS.ByteString -> BS.ByteString -> Bool
constantTimeCompare a b =
    BS.length a == BS.length b &&
        0 == foldl'
            (\acc (x, y) -> acc Bits..|. Bits.xor x y)
            (0 :: Word.Word8)
            (B.zip a b)

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

mkConfig :: AbsDir -> Config
mkConfig dir = Config
    ( mkDeploy
        (AbsDir $ unAbsDir dir FP.</> "hooked-repos")
        (URL "https://www.cordcivilian.com/updated")
    )
    [ mkRepo (RelDir "cord") (URL "https://github.com/cordcivilian/cord.git")
    ]

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

fetchLatestVersion :: Logger -> Config -> Repo -> IO ()
fetchLatestVersion logger config repo = do
    logger $ T.pack "local repo found, fetching latest version"
    Dir.setCurrentDirectory $
        FP.normalise $ getReposDir config FP.</> getLocalRepoDir repo
    isSuccessful <- runCommands
        [ ("git", ["fetch", "-q", "--atomic", getCloneURL repo, "main"])
        , ("git", ["reset", "--hard", "origin/main"])
        , ("git", ["clean", "-dxqf"])
        ]
    case isSuccessful of
      True -> notify logger (getNotifyURL config) repo
      False -> logger $ T.pack "encountered git failure"

notify :: Logger -> String -> Repo -> IO ()
notify logger url repo = do
    currentTime <- Clock.getCurrentTime
    maybeSecret <- Env.lookupEnv "HOOKER"
    initRequest <- HTTP.parseRequest url
    let secret = maybe "" id maybeSecret
        hourlyVersion = ['a'..'z'] !!
            (read $
                DateTimeFormat.formatTime
                DateTimeFormat.defaultTimeLocale
                "%H"
                currentTime
            )
        dailyVersion =
            DateTimeFormat.formatTime
            DateTimeFormat.defaultTimeLocale
            "%Y-%m-%d"
            currentTime
        body = JSON.encode $
            Notification (getHtmlURL repo) (dailyVersion ++ [hourlyVersion])
        signature = [BS.pack "sha256=", sign body secret]
        request =
            HTTP.setRequestMethod "POST" $
            HTTP.setRequestSecure True $
            HTTP.setRequestHeaders
                [ (Headers.hContentType, BS.pack "application/json")
                , (Headers.hContentLength, BS.pack $ show $ BSL.length body)
                , ("Hooker-Signature-256", BS.concat signature)
                ] $
            HTTP.setRequestBodyLBS body $
            initRequest
    response <- HTTP.httpLBS request
    case HTTP.getResponseStatusCode response of
      200 -> logger $ T.pack "hook event notified"
      _ -> logger $ T.pack "invalid hook notification"

syncRepo :: Logger -> Config -> Repo -> IO ()
syncRepo logger config repo = do
    let dir = FP.normalise $ getReposDir config FP.</> getLocalRepoDir repo
    exists <- Dir.doesDirectoryExist dir
    case exists of
      True -> fetchLatestVersion logger config repo
      False -> logger $ T.pack $ "repo dir " ++ dir ++ " does not exist"

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    maybePort <- Env.lookupEnv "PORT"
    let autoPort = 8888
        port = maybe autoPort read maybePort
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    home <- Dir.getHomeDirectory
    config <- IOR.newIORef $ mkConfig $ AbsDir home
    Warp.run port $ monolith config
