{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Implement the git http protocol (smart only):
-- https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols
-- https://github.com/git/git/blob/master/Documentation/technical/http-protocol.txt
--
-- Verbose git clone over http:
-- GIT_CURL_VERBOSE=1 git clone http://127.0.0.1:8080/teh/test


module Holborn.Repo.HttpProtocol
       ( repoServer
       , repoAPI
       ) where

import           BasicPrelude

import           Blaze.ByteString.Builder (Builder, fromByteString)
import           Control.Monad.Trans.Either (EitherT)
import qualified Data.ByteString as BS
import           Network.HTTP.Types.Header (hContentEncoding, RequestHeaders)
import           Network.HTTP.Types.Status (ok200)
import           Network.Wai (Application, responseStream, requestBody, requestHeaders, Request, Response)
import           Pipes ((>->), await, yield)
import           Pipes.Core (Consumer, Producer, Pipe)
import           Pipes.GZip (decompress)
import           Pipes.Safe (SafeT)
import           Pipes.Shell (pipeCmd, producerCmd, runShell, (>?>))
import           Servant ((:>), (:<|>)(..), Get, Capture, QueryParam, Proxy(..), ServantErr, Server, Raw)
import           Text.Printf (printf)

import Holborn.Repo.Config (Config, buildRepoPath)

-- | The git pull & push repository API. The URL schema is borrowed
-- from github, i.e. `/user/repo` or `/org/repo`.
type RepoAPI =
    Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> Get '[] ()
    :<|> Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> "info" :> "refs"
        :> QueryParam "service" Text -- `git-upload-pack` or `git-receive-pack`
        :> Raw
    :<|> Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> "git-upload-pack"
        :> Raw
    :<|> Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> "git-receive-pack"
        :> Raw

repoAPI :: Proxy RepoAPI
repoAPI = Proxy

repoServer :: Config -> Server RepoAPI
repoServer config =
    showNormal
    :<|> (smartHandshake config)
    :<|> (gitUploadPack config)
    :<|> (gitReceivePack config)

-- | Placeholder for "normal" HTTP traffic - without a service. This
-- is where we plug in holborn-web output.
showNormal :: Text -> Text -> EitherT ServantErr IO ()
showNormal _userOrOrg _repo = return ()

backupResponse :: Response
backupResponse = terror "I have no idea whether we can reach this state."

-- | Render in pkg format (4 byte hex prefix for total line length
-- including header)
pktString :: (IsString s) => String -> s
pktString s =
    fromString (printf "%04x" ((length s) + 4) ++ s)

acceptGzip :: RequestHeaders -> Bool
acceptGzip = any ( == (hContentEncoding, "gzip"))

smartHandshake :: Config -> Text -> Text -> Maybe Text -> Application
smartHandshake config userOrOrg repo service =
    localrespond
  where
    repoPath = buildRepoPath config userOrOrg repo

    localrespond :: Application
    localrespond _req respond = do
        liftIO $ print service
        liftIO $ print repoPath

        respond $ case service of
            Just "git-upload-pack" -> gitResponse "git-upload-pack"
            Just "git-receive-pack" -> gitResponse "git-receive-pack"
            _ -> backupResponse

    gitResponse :: String -> Response
    gitResponse serviceName = responseStream ok200 (gitHeaders (fromString serviceName)) (gitPack serviceName)

    gitPack :: String -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack service moreData _flush =
        runShell $ (
            (banner service)
            >> (producerCmd (service ++ " --stateless-rpc --advertise-refs " ++ repoPath) >-> filterStdErr)
            >> footer) >-> sendChunks
      where
        sendChunks :: Consumer ByteString (SafeT IO) ()
        sendChunks = do
            chunk <- await
            liftIO $ moreData (fromByteString chunk)
            sendChunks

    gitHeaders service =
        [ ("Content-Type", "application/x-" ++ service ++ "-advertisement")
        , ("Pragma", "no-cache")
        , ("Server", "holborn")
        , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
        ]

    -- git requires a service header that just repeats the service it
    -- asked for. It also requires `0000` to indicate a boundary.
    banner service = do
        yield (pktString ("# service=" ++ service ++ "\n"))
        yield "0000"
    -- Yield an empty footer to be explicit about what we are not
    -- sending.
    footer = do
        yield ""

-- | The shell library we are using to invoke git returns a Left for
-- stderr output and a Right for stdout output. We don't expect stderr
-- (Left) output in normal operation so we error out (for now).
filterStdErr :: Pipe (Either ByteString ByteString) ByteString (SafeT IO) ()
filterStdErr = do
    x <- await
    case x of
         -- TODO send errors to journald && maybe measure
         Left err -> terror (decodeUtf8 err)
         Right data' -> do
             yield data'
             filterStdErr


producerRequestBody :: Request -> Producer ByteString (SafeT IO) ()
producerRequestBody req =
    case acceptGzip (requestHeaders req) of
        True -> decompress loop
        False -> loop
  where
    loop = do
        data' <- liftIO (requestBody req)
        unless (BS.null data') $ do
            yield data'
            loop


gitReceivePack :: Config -> Text -> Text -> Application
gitReceivePack config userOrOrg repo =
    localrespond
  where
    repoPath = buildRepoPath config userOrOrg repo
    localrespond :: Application
    localrespond req respond = do
        liftIO $ print userOrOrg
        liftIO $ print repo
        respond $ responseStream ok200 headers (gitPack "git-receive-pack" (producerRequestBody req))

    headers =
        [ ("Content-Type", "application/x-git-receive-pack-result")
        , ("Pragma", "no-cache")
        , ("Server", "holborn")
        , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
        ]

    gitPack :: String -> Producer ByteString (SafeT IO) () -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack service postDataProducer moreData _flush =
        runShell $
        postDataProducer >?> pipeCmd (service ++ " --stateless-rpc " ++ repoPath) >-> filterStdErr
            >-> sendChunks
      where
        sendChunks :: Consumer ByteString (SafeT IO) ()
        sendChunks = do
            chunk <- await
            liftIO $ moreData (fromByteString chunk)
            sendChunks


gitUploadPack :: Config -> Text -> Text -> Application
gitUploadPack config userOrOrg repo =
    localrespond
  where
    repoPath = buildRepoPath config userOrOrg repo
    localrespond :: Application
    localrespond req respond = do
        liftIO $ print userOrOrg
        liftIO $ print repo
        respond $ responseStream ok200 headers (gitPack "git-upload-pack" (producerRequestBody req))
        -- todo header checking

    headers =
        [ ("Content-Type", "application/x-git-upload-pack-result")
        , ("Pragma", "no-cache")
        , ("Server", "holborn")
        , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
        ]

    gitPack :: String -> Producer ByteString (SafeT IO) () -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack service postDataProducer moreData _flush =
        runShell $
        postDataProducer >?> pipeCmd (service ++ " --stateless-rpc " ++ repoPath) >-> filterStdErr
            >-> sendChunks
      where
        sendChunks :: Consumer ByteString (SafeT IO) ()
        sendChunks = do
            chunk <- await
            liftIO $ moreData (fromByteString chunk)
            sendChunks
