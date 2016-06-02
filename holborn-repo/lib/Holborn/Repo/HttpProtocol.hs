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
       ( repoServer
       , repoAPI
       ) where

import           BasicPrelude

import           Blaze.ByteString.Builder (Builder, fromByteString)
import qualified Data.ByteString as BS
import           Network.HTTP.Types.Header (hContentEncoding, RequestHeaders)
import           Network.HTTP.Types.Status (ok200)
import           Network.Wai (Application, responseStream, requestBody, requestHeaders, Request, Response)
import           Pipes ((>->), await, yield)
import           Pipes.Core (Consumer, Producer, Pipe)
import           Pipes.GZip (decompress)
import           Pipes.Safe (SafeT)
import           Pipes.Shell (pipeCmd, producerCmd, runShell, (>?>))
import           Servant ((:>), (:<|>)(..), Capture, QueryParam, Proxy(..), Server, Raw)
import           Text.Printf (printf)
import           Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

import Holborn.Repo.Browse (BrowseAPI, codeBrowser)
import Holborn.Repo.Config (Config, buildRepoPath)
import Holborn.Repo.GitLayer (makeRepository)
import Holborn.JSON.RepoMeta (newValidRepoName)
import Holborn.JSON.RepoMeta (RepoId)


-- | The git pull & push repository API. The URL schema is borrowed
-- from github, i.e. `/user/repo` or `/org/repo`.
type RepoAPI =
    "v1"
    :> "repos"
    :> Capture "repoId" RepoId
    :> (BrowseAPI :<|> GitProtocolAPI)


repoAPI :: Proxy RepoAPI
repoAPI = Proxy


repoServer :: Config -> Server RepoAPI
repoServer config repoId =
    codeBrowser repo :<|> gitProtocolAPI repoPath
    where
      repo = makeRepository repoId repoPath
      repoPath = buildRepoPath config repoId


-- | The core git protocol for a single repository.
type GitProtocolAPI =
       "info" :> "refs" :> QueryParam "service" GitService :> Raw
  :<|> "git-upload-pack" :> Raw
  :<|> "git-receive-pack" :> Raw


-- | Git offers two kinds of service.
data GitService = GitUploadPack | GitReceivePack

stringyService :: IsString a => GitService -> a
stringyService serviceType = case serviceType of
  GitUploadPack -> "git-upload-pack"
  GitReceivePack -> "git-receive-pack"

instance ToHttpApiData GitService where
    toUrlPiece = stringyService

instance FromHttpApiData GitService where
    parseUrlPiece "git-upload-pack" = pure GitUploadPack
    parseUrlPiece "git-receive-pack" = pure GitReceivePack
    parseUrlPiece _ = Left "Invalid git service"


data GitResponse = Service | Advertisement


gitProtocolAPI :: FilePath -> Server GitProtocolAPI
gitProtocolAPI repoPath =
  smartHandshake repoPath
  :<|> gitServe GitUploadPack repoPath
  :<|> gitServe GitReceivePack repoPath


smartHandshake :: FilePath -> Maybe GitService -> Application
smartHandshake repoPath service =
    handshakeApp
  where
    handshakeApp :: Application
    handshakeApp _req respond =
        respond $ maybe backupResponse gitResponse service

    gitResponse :: GitService -> Response
    gitResponse serviceType =
        responseStream
            ok200
            (gitHeaders serviceType Advertisement)
            (gitPack' serviceType)

    backupResponse :: Response
    backupResponse = terror "no git service specified: I have no idea whether we can reach this state."

    gitPack' :: GitService -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack' serviceType moreData _flush =
        runShell $ (
            banner serviceName
            >> (producerCmd (serviceName ++ " --stateless-rpc --advertise-refs " ++ repoPath) >-> filterStdErr)
            >> footer) >-> sendChunks moreData
      where

        serviceName = stringyService serviceType

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


gitServe :: GitService -> FilePath -> Application
gitServe serviceType repoPath = localrespond
  where
    localrespond :: Application
    localrespond req respond =
        respond $ responseStream
          ok200
          (gitHeaders serviceType Service)
          (gitPack repoPath serviceType (producerRequestBody req))


gitHeaders :: GitService -> GitResponse -> RequestHeaders
gitHeaders serviceType gitResponse =
    [ ("Content-Type", "application/x-" ++ service ++ "-" ++ suffix)
    , ("Pragma", "no-cache")
    , ("Server", "holborn")
    , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
    ]
  where
    service = stringyService serviceType
    suffix = case gitResponse of
      Service -> "service"
      Advertisement -> "advertisement"


gitPack :: FilePath -> GitService -> Producer ByteString (SafeT IO) () -> (Builder -> IO ()) -> IO () -> IO ()
gitPack repoPath serviceType postDataProducer moreData _flush =
    runShell $
    postDataProducer >?> pipeCmd (service ++ " --stateless-rpc " ++ repoPath) >-> filterStdErr
        >-> sendChunks moreData
  where
    service = stringyService serviceType


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
