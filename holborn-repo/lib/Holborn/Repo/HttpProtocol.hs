{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Implement the git http protocol (smart only):
-- https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols
-- https://gist.github.com/schacon/6092633
--
-- Verbose git clone over http:
-- GIT_CURL_VERBOSE=1 git clone http://127.0.0.1:8080/teh/test


module Holborn.Repo.HttpProtocol
       ( repoServer
       , repoAPI
       ) where

import BasicPrelude

import Servant ((:>), (:<|>)(..), Get, Capture, QueryParam, Proxy(..), ServantErr, Server, Raw)
import Control.Monad.Trans.Either (EitherT)
import Network.HTTP.Types.Status (ok200)
import Network.Wai (responseLBS, Application, responseStream, requestBody, Request, Response)
import Pipes.Core (Consumer, Producer, Pipe)
import Pipes ((>->), await, yield)
import Pipes.Shell (pipeCmd, producerCmd, runShell, (>?>))
import Pipes.Safe (SafeT)
import Blaze.ByteString.Builder (Builder, fromByteString)
import Text.Printf (printf)
import qualified Data.ByteString as BS

type RepoAPI =
    Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> Get '[] ()
    :<|> Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> "info" :> "refs"
        :> QueryParam "service" Text
        :> Raw
    :<|> Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> "git-upload-pack"
        :> Raw

repoAPI :: Proxy RepoAPI
repoAPI = Proxy

repoServer :: Server RepoAPI
repoServer =
    showHead
    :<|> smartHandshake
    :<|> gitUploadPack

showHead :: Text -> Text -> EitherT ServantErr IO ()
showHead userOrOrg repo = return ()

backupResponse :: Response
backupResponse = responseLBS ok200 [] "todo - make this an error"

-- | Render in pkg format (4 byte hex prefix for total line length
-- including header)
pktString :: (IsString s) => String -> s
pktString s =
    fromString (printf "%04x" ((length s) + 4) ++ s)


smartHandshake :: Text -> Text -> Maybe Text -> Server Raw
smartHandshake userOrOrg repo service =
    localrespond
  where
    localrespond :: Application
    localrespond req respond = do
        liftIO $ print userOrOrg
        liftIO $ print repo
        liftIO $ print service
        respond $ case service of
            Nothing -> backupResponse
            Just "git-upload-pack" ->
                responseStream ok200 (gitHeaders "git-upload-pack") (gitPack "git-upload-pack")
            Just "git-receive-pack" ->
                responseStream ok200 (gitHeaders "git-receive-pack") (gitPack "git-receive-pack")
            Just _ -> backupResponse

    gitPack :: String -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack service moreData flush =
        runShell $ (
            (banner service)
            >> (producerCmd (service ++ " --stateless-rpc --advertise-refs /tmp/g0/") >-> filterStdErr)
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
        , ("Server", "git")
        , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
        ]

    -- git requires a service header that just repeats the service it
    -- asked for. It also requires `0000` to indicate a boundary.
    banner service = do
        yield (pktString ("# service=" ++ service ++ "\n"))
        yield "0000"
    -- protocol requires messages end with 0000
    footer = do
        yield ""


filterStdErr :: Pipe (Either ByteString ByteString) ByteString (SafeT IO) ()
filterStdErr = do
    x <- await
    case x of
         -- TODO send errors to journald && maybe measure
         Left err -> terror (decodeUtf8 err)
         Right data_ -> do
             yield data_
             filterStdErr


producerRequestBody :: Request -> Producer ByteString (SafeT IO) ()
producerRequestBody req =
    loop
  where
    loop = do
        data_ <- liftIO (requestBody req)
        unless (BS.null data_) $ do
            yield data_
            loop


gitUploadPack :: Text -> Text -> Server Raw
gitUploadPack userOrOrg repo =
    localrespond
  where
    localrespond :: Application
    localrespond req respond = do
        liftIO $ print userOrOrg
        liftIO $ print repo
        respond $ responseStream ok200 headers (gitPack "git-upload-pack" (producerRequestBody req))
        -- todo header checking

    headers =
        [ ("Content-Type", "application/x-git-upload-pack-result")
        , ("Pragma", "no-cache")
        , ("Server", "git")
        , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
        ]

    gitPack :: String -> Producer ByteString (SafeT IO) () -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack service postDataProducer moreData flush =
        runShell $
        postDataProducer >?> pipeCmd (service ++ " --stateless-rpc /tmp/g0/") >-> filterStdErr
            >-> sendChunks
      where
        sendChunks :: Consumer ByteString (SafeT IO) ()
        sendChunks = do
            chunk <- await
            liftIO $ moreData (fromByteString chunk)
            sendChunks
