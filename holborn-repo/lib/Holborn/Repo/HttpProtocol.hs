{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Implement the git http protocol (smart only):
-- https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols
-- https://github.com/git/git/blob/master/Documentation/technical/http-protocol.txt
--
-- Verbose git clone over http:
-- GIT_CURL_VERBOSE=1 git clone http://127.0.0.1:8080/teh/test


module Holborn.Repo.HttpProtocol
       ( API
       , server
       ) where

import           HolbornPrelude

import           Blaze.ByteString.Builder (Builder, fromByteString)
import qualified Data.ByteString as BS
import           Data.List (lookup)
import           Network.HTTP.Types.Header (hContentEncoding, RequestHeaders)
import           Network.HTTP.Types.Status (ok200)
import           Network.Wai (Application, responseStream, requestBody, requestHeaders, Request, Response)
import           Pipes ((>->), await, yield)
import           Pipes.Core (Consumer, Producer, Pipe)
import           Pipes.GZip (decompress)
import           Pipes.Safe (SafeT)
import           Pipes.Shell (pipeCmd, producerCmd, runShell, (>?>))
import           Servant ((:>), (:<|>)(..), QueryParam, Server, Raw)
import           Text.Printf (printf)
import           Web.HttpApiData (ToHttpApiData(..))

import Holborn.JSON.SSHRepoCommunication (GitCommand(..), unparseGitCommand)
import Holborn.Repo.Filesystem (DiskLocation, diskLocationToPath, repoInit)


-- | The core git protocol for a single repository.
type API =
       "info" :> "refs" :> QueryParam "service" GitCommand :> Raw
  :<|> "git-upload-pack" :> Raw
  :<|> "git-receive-pack" :> Raw


data GitResponse = Service | Advertisement


server :: DiskLocation -> Server API
server diskLocation =
  smartHandshake diskLocation
  :<|> gitServe GitUploadPack diskLocation
  :<|> gitServe GitReceivePack diskLocation


smartHandshake :: DiskLocation -> Maybe GitCommand -> Application
smartHandshake diskLocation service =
    handshakeApp
  where
    handshakeApp :: Application
    handshakeApp _req respond = do
        -- NB we're lazy-initing on both push and pull, so we allow
        -- cloning of empty repositories. The user will see:
        -- "warning: You appear to have cloned an empty repository."
        -- TODO: error logging
        void (repoInit diskLocation)
        respond $ maybe backupResponse gitResponse service

    gitResponse :: GitCommand -> Response
    gitResponse serviceType =
        responseStream
            ok200
            (gitHeaders serviceType Advertisement)
            (gitPack' serviceType)

    backupResponse :: Response
    backupResponse = terror "no git service specified: I have no idea whether we can reach this state."

    gitPack' :: GitCommand -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack' serviceType moreData _flush =
        runShell $ (
            banner serviceName
            >> (producerCmd (serviceName ++ " --stateless-rpc --advertise-refs " ++ (diskLocationToPath diskLocation)) >-> filterStdErr)
            >> footer) >-> sendChunks moreData
      where

        serviceName = unparseGitCommand serviceType

    -- git requires a service header that just repeats the service it
    -- asked for. It also requires `0000` to indicate a boundary.
    banner serviceName = do
        yield (pktString ("# service=" ++ serviceName ++ "\n"))
        yield "0000"

    -- | Render in pkg format (4 byte hex prefix for total line length
    -- including header)
    pktString :: (IsString s) => String -> s
    pktString s =
        fromString (printf "%04x" (length s + 4) ++ s)

    -- Yield an empty footer to be explicit about what we are not
    -- sending.
    footer = yield ""


gitServe :: GitCommand -> DiskLocation -> Application
gitServe serviceType diskLocation = localrespond
  where
    localrespond :: Application
    localrespond req respond =
        respond $ responseStream
          ok200
          (gitHeaders serviceType Service)
          (gitPack diskLocation serviceType (producerRequestBody req))


gitHeaders :: GitCommand -> GitResponse -> RequestHeaders
gitHeaders serviceType gitResponse =
    [ ("Content-Type", "application/x-" ++ service ++ "-" ++ suffix)
    , ("Pragma", "no-cache")
    , ("Server", "holborn")
    , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
    ]
  where
    service = toHeader serviceType
    suffix = case gitResponse of
      Service -> "service"
      Advertisement -> "advertisement"


gitPack :: DiskLocation -> GitCommand -> Producer ByteString (SafeT IO) () -> (Builder -> IO ()) -> IO () -> IO ()
gitPack diskLocation serviceType postDataProducer moreData _flush =
    runShell $
    postDataProducer >?> pipeCmd (service ++ " --stateless-rpc " ++ (diskLocationToPath diskLocation)) >-> filterStdErr
        >-> sendChunks moreData
  where
    service = unparseGitCommand serviceType


sendChunks :: (Builder -> IO ()) -> Consumer ByteString (SafeT IO) ()
sendChunks moreData = do
    chunk <- await
    liftIO $ moreData (fromByteString chunk)
    sendChunks moreData


producerRequestBody :: Request -> Producer ByteString (SafeT IO) ()
producerRequestBody req =
    case getContentEncoding req of
        Just "gzip" -> decompress loop
        _ -> loop
  where
    loop = do
        data' <- liftIO (requestBody req)
        unless (BS.null data') $ do
            yield data'
            loop


getContentEncoding :: Request -> Maybe ByteString
getContentEncoding = lookup hContentEncoding . requestHeaders


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
