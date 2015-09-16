{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Implement the git http protocol (smart only):
-- https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols
--
-- Verbose git clone over http:
-- GIT_CURL_VERBOSE=1 git clone http://127.0.0.1:8080/teh/test


module Holborn.Repo.HttpProtocol
       ( repoServer
       , repoAPI
       ) where

import BasicPrelude

import Servant ((:>), (:<|>)(..), Get, Post, Capture, QueryParam, Proxy(..), ServantErr, Server, OctetStream, Raw)
import Control.Monad.Trans.Either (EitherT)
import Pipes.Wai (responseRawProducer)
import Network.HTTP.Types.Status (ok200)
import Network.Wai (responseLBS, Application)
import Pipes.Core (Consumer, Producer, Pipe)
import Pipes ((>->), runEffect, await, yield, (>~))
import Pipes.Shell (pipeCmd, runShell, (>?>))
import Pipes.Safe (SafeT)

type RepoAPI =
    Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> Get '[] ()
    :<|> Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> "info" :> "refs"
        :> QueryParam "service" Text
        :> Raw


repoAPI :: Proxy RepoAPI
repoAPI = Proxy

showHead :: Text -> Text -> EitherT ServantErr IO ()
showHead userOrOrg repo = return ()

backupResponse = responseLBS ok200 [] "problem?"

initiateProtocol :: Text -> Text -> Maybe Text -> Server Raw
initiateProtocol userOrOrg repo service =
    localrespond
  where
    localrespond :: Application
    localrespond req respond = do
        liftIO $ print "localrespond"
        liftIO $ print userOrOrg
        liftIO $ print repo
        liftIO $ print service
        case service of
         Nothing -> respond backupResponse
         Just "git-upload-pack" -> respond (responseRawProducer (gitPack "git-upload-pack") backupResponse)
         Just "git-receive-pack" -> respond (responseRawProducer (gitPack "git-receive-pack") backupResponse)

    gitPack :: String -> Producer ByteString IO () -> Consumer ByteString (SafeT IO) () -> IO ()
    gitPack command _ outgoingData =
        runShell $ (headers >> (yield "" >?> pipeCmd (command ++ " /home/tom/testbed/g0") >-> outgoing)) >-> outgoingData

    headers = do
        yield "HTTP/1.1 200 OK\r\n"
	yield "Content-Type: application/x-git-upload-pack-advertisement\r\n"
        yield "Transfer-Encoding: chunked\r\n"
	yield "Cache-Control: no-cache\r\n\r\n"
        yield "001e# service=git-upload-pack\n"

outgoing :: (MonadIO m) => Pipe (Either ByteString ByteString) ByteString m ()
outgoing = do
    x <- await
    -- TODO log stderr to journald
    liftIO $ print x
    case x of
     Left err -> yield ""
     Right data_ -> yield data_

repoServer :: Server RepoAPI
repoServer =
    showHead
    :<|> initiateProtocol
