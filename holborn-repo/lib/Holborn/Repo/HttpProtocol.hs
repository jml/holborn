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
import           Servant.Common.Text (FromText(..), ToText(..))
import           Text.Printf (printf)

import Holborn.Repo.Config (Config, buildRepoPath)

-- | The git pull & push repository API. The URL schema is borrowed
-- from github, i.e. `/user/repo` or `/org/repo`.

data GitService = GitUploadPack | GitReceivePack

instance ToText GitService where
    toText GitUploadPack = "git-upload-pack"
    toText GitReceivePack = "git-receive-pack"

instance FromText GitService where
    fromText "git-upload-pack" = Just GitUploadPack
    fromText "git-receive-pack" = Just GitReceivePack
    fromText _ = Nothing


type GitProtocolAPI =
       "info" :> "refs" :> QueryParam "service" GitService :> Raw
  :<|> "git-upload-pack" :> Raw
  :<|> "git-receive-pack" :> Raw


type RepoAPI =
    Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> ( Get '[] () :<|> GitProtocolAPI)


repoAPI :: Proxy RepoAPI
repoAPI = Proxy


repoServer :: Config -> Server RepoAPI
repoServer config userOrOrg repo =
    showNormal :<|> gitProtocolAPI repoPath
    where
      repoPath = buildRepoPath config userOrOrg repo


gitProtocolAPI :: FilePath -> Server GitProtocolAPI
gitProtocolAPI repoPath =
  (smartHandshake repoPath)
  :<|> (gitUploadPack repoPath)
  :<|> (gitReceivePack repoPath)


-- | Placeholder for "normal" HTTP traffic - without a service. This
-- is where we plug in holborn-web output.
showNormal :: EitherT ServantErr IO ()
showNormal = return ()


backupResponse :: Response
backupResponse = terror "I have no idea whether we can reach this state."


-- | Render in pkg format (4 byte hex prefix for total line length
-- including header)
pktString :: (IsString s) => String -> s
pktString s =
    fromString (printf "%04x" ((length s) + 4) ++ s)


smartHandshake :: FilePath -> Maybe GitService -> Application
smartHandshake repoPath service =
    handshakeApp
  where
    handshakeApp :: Application
    handshakeApp _req respond =
        respond $ maybe backupResponse (gitResponse . toText) service

    gitResponse :: Text -> Response
    gitResponse serviceName =
        responseStream ok200 (gitHeaders' (encodeUtf8 serviceName)) (gitPack' (textToString serviceName))

    gitHeaders' serviceName =
        [ ("Content-Type", "application/x-" ++ serviceName ++ "-advertisement")
        , ("Pragma", "no-cache")
        , ("Server", "holborn")
        , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
        ]

    gitPack' :: String -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack' serviceName moreData _flush =
        runShell $ (
            (banner serviceName)
            >> (producerCmd (serviceName ++ " --stateless-rpc --advertise-refs " ++ repoPath) >-> filterStdErr)
            >> footer) >-> sendChunks
      where
        sendChunks :: Consumer ByteString (SafeT IO) ()
        sendChunks = do
            chunk <- await
            liftIO $ moreData (fromByteString chunk)
            sendChunks

    -- git requires a service header that just repeats the service it
    -- asked for. It also requires `0000` to indicate a boundary.
    banner serviceName = do
        yield (pktString ("# service=" ++ serviceName ++ "\n"))
        yield "0000"

    -- Yield an empty footer to be explicit about what we are not
    -- sending.
    footer = do
        yield ""


gitReceivePack :: FilePath -> Application
gitReceivePack repoPath =
    localrespond
  where
    localrespond :: Application
    localrespond req respond = do
        respond $ responseStream ok200 headers (gitPack repoPath "git-receive-pack" (producerRequestBody req))

    headers = gitHeaders GitReceivePack


gitUploadPack :: FilePath -> Application
gitUploadPack repoPath =
    localrespond
  where
    localrespond :: Application
    localrespond req respond = do
        respond $ responseStream ok200 headers (gitPack repoPath "git-upload-pack" (producerRequestBody req))
        -- todo header checking

    headers = gitHeaders GitUploadPack


gitHeaders :: GitService -> RequestHeaders
gitHeaders serviceType =
    [ ("Content-Type", "application/x-" ++ encodeUtf8 (toText serviceType) ++ "-result")
    , ("Pragma", "no-cache")
    , ("Server", "holborn")
    , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
    ]


gitPack :: FilePath -> String -> Producer ByteString (SafeT IO) () -> (Builder -> IO ()) -> IO () -> IO ()
gitPack repoPath service postDataProducer moreData _flush =
    runShell $
    postDataProducer >?> pipeCmd (service ++ " --stateless-rpc " ++ repoPath) >-> filterStdErr
        >-> sendChunks
  where
    sendChunks :: Consumer ByteString (SafeT IO) ()
    sendChunks = do
        chunk <- await
        liftIO $ moreData (fromByteString chunk)
        sendChunks


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


acceptGzip :: RequestHeaders -> Bool
acceptGzip = any ( == (hContentEncoding, "gzip"))


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
