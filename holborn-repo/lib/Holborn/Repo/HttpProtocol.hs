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
       , diskLocationToPath
       , DiskLocation(..)
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
import Holborn.Repo.Config (Config(..))
import Holborn.Repo.GitLayer (makeRepository)
import Holborn.JSON.RepoMeta (RepoId)
import Holborn.Repo.RepoInit (repoInit)


-- | The git pull & push repository API. The URL schema is borrowed
-- from github, i.e. `/user/repo` or `/org/repo`.
type RepoAPI =
    "v1"
    :> "repos"
    :> Capture "repoId" RepoId
    :> (BrowseAPI :<|> GitProtocolAPI)


repoAPI :: Proxy RepoAPI
repoAPI = Proxy


-- | Data type to describe where the repository lives on disk. Will
-- probably be extended to handle implicit clones.
data DiskLocation = DiskLocation { repoRoot :: FilePath, repoId :: RepoId }

diskLocationToPath :: DiskLocation -> String
diskLocationToPath DiskLocation{..} = repoRoot <> "/" <> textToString (toUrlPiece repoId)


repoServer :: Config -> Server RepoAPI
repoServer Config{repoRoot} repoId =
    codeBrowser repo :<|> gitProtocolAPI diskLocation
    where
      diskLocation = DiskLocation repoRoot repoId
      repo = makeRepository repoId (diskLocationToPath diskLocation)


-- | The core git protocol for a single repository.
type GitProtocolAPI =
       "info" :> "refs" :> QueryParam "service" GitService :> Raw
  :<|> "git-upload-pack" :> Raw
  :<|> "git-receive-pack" :> Raw



-- | Git offers two kinds of service.
-- TODO: unify with the GitUploadPack etc stuff in SSHRepoCommunication
-- by moving it to:
-- data GitCommand = GitReceivePack | GitUploadPack
-- data SSHCommandLine = SSHCommandLine GitCommand Text ValidRepoName
data GitService = GitUploadPack | GitReceivePack deriving (Show)

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


gitProtocolAPI :: DiskLocation -> Server GitProtocolAPI
gitProtocolAPI diskLocation =
  smartHandshake diskLocation
  :<|> gitServe GitUploadPack diskLocation
  :<|> gitServe GitReceivePack diskLocation




smartHandshake :: DiskLocation -> Maybe GitService -> Application
smartHandshake diskLocation@DiskLocation{..} service =
    handshakeApp
  where
    handshakeApp :: Application
    handshakeApp _req respond = do
        -- NB we're lazy-initing on both push and pull, so we allow
        -- cloning of empty repositories. The user will see:
        -- "warning: You appear to have cloned an empty repository."
        -- TODO: error logging
        void (repoInit repoRoot repoId)
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
            >> (producerCmd (serviceName ++ " --stateless-rpc --advertise-refs " ++ (diskLocationToPath diskLocation)) >-> filterStdErr)
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


gitServe :: GitService -> DiskLocation -> Application
gitServe serviceType diskLocation = localrespond
  where
    localrespond :: Application
    localrespond req respond =
        respond $ responseStream
          ok200
          (gitHeaders serviceType Service)
          (gitPack diskLocation serviceType (producerRequestBody req))


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


gitPack :: DiskLocation -> GitService -> Producer ByteString (SafeT IO) () -> (Builder -> IO ()) -> IO () -> IO ()
gitPack diskLocation serviceType postDataProducer moreData _flush =
    runShell $
    postDataProducer >?> pipeCmd (service ++ " --stateless-rpc " ++ (diskLocationToPath diskLocation)) >-> filterStdErr
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
