module Main (main) where

-- import System.Environment (lookupEnv)
-- import Text.Read (readMaybe)
-- import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import qualified System.Process as Process
import qualified System.Directory as Dir

hooks :: Wai.Application
hooks request respond = do
    body <- Wai.lazyRequestBody request
    respond $ case (request_method, request_path) of
        ("GET", "/") -> rootRoute request
        ("GET", "/hooked/list") -> undefined
        ("GET", "/hooked/logs") -> undefined
        ("POST", "/events") -> hookRoute request body
        ("POST", "/test") -> testRoute request
        _ -> notFoundRoute
        where
            request_method = BS.unpack $ Wai.requestMethod request
            request_path = BS.unpack $ Wai.rawPathInfo request

loggedHooks :: Wai.Application
loggedHooks = Mid.logStdout hooks

rootRoute :: Wai.Request -> Wai.Response
rootRoute request = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (
        BSL.concat [
            BSL.fromStrict $ Wai.requestMethod request,
            BSL.pack "\n",
            BSL.pack "Github Webhook Base"
                   ]
    )

testRoute :: Wai.Request -> Wai.Response
testRoute request = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack $ show $ Wai.requestBodyLength request)

hookRoute :: Wai.Request -> BSL.ByteString -> Wai.Response
hookRoute request body = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (
        BSL.concat [
            body,
            BSL.pack "\n",
            BSL.pack $ show $ Wai.queryString request,
            BSL.pack "\n",
            BSL.pack $ show $ Wai.requestHeaders request
                   ]
    )

notFoundRoute :: Wai.Response
notFoundRoute = Wai.responseLBS
    HTTP.status404
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack "404 - Not Found")

-- ---------------------------------------------------------------------------

newtype AbsDir = AbsDir { unAbsDir :: String } deriving (Eq, Show)
newtype RelDir = RelDir { unRelDir :: String } deriving (Eq, Show)
newtype URL = URL { unURL :: String }  deriving (Eq, Show)

newtype ReposDir = ReposDir
    { unReposDir :: AbsDir } deriving (Eq, Show)
newtype LocalRepoDir = LocalRepoDir
    { unLocalRepoDir :: RelDir } deriving (Eq, Show)

newtype NotifyURL = NotifyURL
    { unNotifyURL :: URL } deriving (Eq, Show)
newtype RemoteRepoURL = RemoteRepoURL
    { unRemoteRepoURL :: URL } deriving (Eq, Show)

data Deploy = Deploy
    { reposDir :: ReposDir , notifyURL :: NotifyURL } deriving (Eq, Show)

data Repo = Repo 
    { localRepoDir :: LocalRepoDir
    , remoteRepoURL :: RemoteRepoURL
    } deriving (Eq, Show)

data Config = Config
    { deploy :: Deploy
    , repos :: [Repo]
    } deriving (Eq, Show)

-- ---------------------------------------------------------------------------

mkRepo :: RelDir -> URL -> Repo
mkRepo dir url = Repo (LocalRepoDir dir) (RemoteRepoURL url)

mkDeploy :: AbsDir -> URL -> Deploy
mkDeploy dir url = Deploy (ReposDir dir) (NotifyURL url)

config :: Config
config = Config
    ( mkDeploy (AbsDir "/Users/joshwong/repos") (URL "https://www.cordcivilian.com/update") )
    [ mkRepo (RelDir "cord") (URL "https://github.com/cordcivilian/cord.git")
    ]

getReposDir :: FilePath
getReposDir = unAbsDir $ unReposDir $ reposDir $ deploy $ config

getLocalRepoDir :: Repo -> FilePath
getLocalRepoDir r = unRelDir $ unLocalRepoDir $ localRepoDir $ r

getRemoteRepoURL :: Repo -> String
getRemoteRepoURL r = unURL $ unRemoteRepoURL $ remoteRepoURL $ r

-- ---------------------------------------------------------------------------

cloneRepo :: URL -> IO ()
cloneRepo remote = do
    putStrLn "local repo not found, cloning"
    Dir.setCurrentDirectory getReposDir
    Process.callProcess "git" ["clone", unURL remote]

fetchLatestVersion :: RelDir -> URL -> IO ()
fetchLatestVersion dir remote = do
    putStrLn "local repo found, fetching latest version"
    Dir.setCurrentDirectory $ unRelDir dir
    Process.callProcess "git" ["fetch", "-q", unURL remote, "main"]
    Process.callProcess "git" ["reset", "--hard", "origin/main"]
    Process.callProcess "git" ["clean", "-dxqf"]

syncRepo :: RelDir -> URL -> IO ()
syncRepo dir remote = do 
    exists <- Dir.doesDirectoryExist $ unRelDir dir
    if exists then fetchLatestVersion dir remote else cloneRepo remote

-- curl -v 'http://127.0.0.1:8888/?123=123' -d testing=123
main :: IO ()
main = do
    -- maybeWebhookSecret <- lookupEnv "HOOKER"
    -- let webhookSecret = maybe "" (fromMaybe "" . readMaybe) maybeWebhookSecret
    let port = 8888
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    Warp.run port loggedHooks
