{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (lookupEnv)

import Data.Bits (xor, (.|.))
import Data.Word (Word8)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Aeson as JSON

import qualified Network.URI as URI

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import qualified System.Process as Process
import qualified System.Directory as Dir
import qualified System.FilePath as FP

hooks :: Logger -> Wai.Application
hooks logger request respond = do
    body <- Wai.lazyRequestBody request
    let request_method = BS.unpack $ Wai.requestMethod request
        request_path = BS.unpack $ Wai.rawPathInfo request
    response <- case (request_method, request_path) of
        ("GET", "/") -> return $ rootRoute request
        ("POST", "/events") -> do
            r <- hookRoute logger request body
            return r
        ("GET", "/hooked/list") -> return $ hookedListRoute request
        ("GET", "/hooked/logs") -> return $ hookedLogsRoute request
        ("POST", "/test") -> return $ testRoute request
        _ -> do
            r <- notFoundRoute logger request body
            return r
    respond response

type Logger = T.Text -> IO ()
loggedHooks :: Wai.Application
loggedHooks = Mid.logStdout $ hooks TIO.putStrLn

rootRoute :: Wai.Request -> Wai.Response
rootRoute request = Wai.responseLBS
    HTTP.status200
    [ (Headers.hContentType, BS.pack "text/plain") ]
    ( BSL.concat [ BSL.fromStrict $ Wai.requestMethod request
                 ,BSL.pack "\n"
                 ,BSL.pack "Github Webhook Base"
                 ] )

hookedListRoute :: Wai.Request -> Wai.Response
hookedListRoute _ = Wai.responseLBS
    HTTP.status200
    [ (Headers.hContentType, BS.pack "text/plain") ]
    (BSL.unlines $ map (BSL.pack . show) (repos config))

-- get all hooked action from memory
hookedLogsRoute :: Wai.Request -> Wai.Response
hookedLogsRoute _ = undefined

testRoute :: Wai.Request -> Wai.Response
testRoute request = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack $ show $ Wai.requestBodyLength request)

hookRoute :: Logger -> Wai.Request -> BSL.ByteString -> IO Wai.Response
hookRoute logger request body = do
    maybeSecret <- lookupEnv "HOOKER"
    let secret = maybe "" id maybeSecret
        signature = lookup "x-hub-signature-256" $ Wai.requestHeaders request
    case (verifySignature body signature secret, getEventInfo body) of
      (Right (), Just event) -> case ref event of 
        "refs/heads/main" -> do 
            let fullName = repoFullName event
                hookedRepo = remoteFullNameToHookedRepo fullName
            mapM_ syncRepo hookedRepo
            logger $ T.pack fullName
        _ -> logger $ T.pack "not a main event"
      (_, _) -> logger $ T.pack "invalid event"
    return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/plain")]
        (BSL.pack "event processed")

notFoundRoute :: Logger -> Wai.Request -> BSL.ByteString -> IO Wai.Response
notFoundRoute logger request body = do
    logger $ T.pack "not found"
    return $ Wai.responseLBS
        HTTP.status404
        [(Headers.hContentType, BS.pack "text/plain")]
        (BSL.pack "404 - Not Found")

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

-- ---------------------------------------------------------------------------

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
        0 == foldl' (\acc (x, y) -> acc .|. xor x y) (0 :: Word8) (B.zip a b)

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
    ( mkDeploy
        (AbsDir $ "/" FP.</> "hooked-repos" )
        (URL "https://www.cordcivilian.com/update")
    )
    [ mkRepo (RelDir "cord") (URL "https://github.com/cordcivilian/cord.git")
    ]

getReposDir :: FilePath
getReposDir = unAbsDir $ unReposDir $ reposDir $ deploy $ config

getLocalRepoDir :: Repo -> FilePath
getLocalRepoDir r = unRelDir $ unLocalRepoDir $ localRepoDir $ r

getRemoteRepoURL :: Repo -> String
getRemoteRepoURL r = unURL $ unRemoteRepoURL $ remoteRepoURL $ r

-- ---------------------------------------------------------------------------

isHookedOnRemote :: String -> Repo -> Bool
isHookedOnRemote fullName repo = 
    case URI.parseURI $ getRemoteRepoURL repo of
      Just uri -> URI.uriPath uri == "/" ++ fullName ++ ".git"
      Nothing -> False

remoteFullNameToHookedRepo :: String -> [Repo]
remoteFullNameToHookedRepo fullName =
    filter (isHookedOnRemote fullName) $ repos config

-- ---------------------------------------------------------------------------

cloneRepo :: Repo -> IO ()
cloneRepo repo = do
    putStrLn "local repo not found, cloning"
    Dir.setCurrentDirectory getReposDir
    Process.callProcess "git" ["clone", getRemoteRepoURL repo]

fetchLatestVersion :: Repo -> IO ()
fetchLatestVersion repo = do
    putStrLn "local repo found, fetching latest version"
    Dir.setCurrentDirectory absPath
    Process.callProcess "git" ["fetch", "-q", getRemoteRepoURL repo, "main"]
    Process.callProcess "git" ["reset", "--hard", "origin/main"]
    Process.callProcess "git" ["clean", "-dxqf"]
        where absPath = FP.normalise $ getReposDir FP.</> getLocalRepoDir repo

syncRepo :: Repo -> IO ()
syncRepo repo = do 
    exists <- Dir.doesDirectoryExist absPath
    if exists then fetchLatestVersion repo else cloneRepo repo
        where absPath = FP.normalise $ getReposDir FP.</> getLocalRepoDir repo

-- ---------------------------------------------------------------------------

-- ioref: homed config 
-- ioref: logs
-- ioref: sync state
main :: IO ()
main = do
    maybePort <- lookupEnv "PORT"
    let autoPort = 8888
        port = maybe autoPort read maybePort
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    Warp.run port loggedHooks
