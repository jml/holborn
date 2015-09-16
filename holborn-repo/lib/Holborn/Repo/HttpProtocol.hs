{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Implement the git http protocol (smart only):
-- https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols

module Holborn.Repo.HttpProtocol
       ( repoServer
       , repoAPI
       ) where

import BasicPrelude

import Servant ((:>), (:<|>)(..), Get, Post, Capture, QueryParam, Proxy(..), ServantErr, Server, OctetStream, Raw)
import Control.Monad.Trans.Either (EitherT)
import Pipes.Wai (responseRawProducer)
import Network.HTTP.Types.Status (ok200)
import Pipes.Cliff (pipeInputOutput, NonPipe(..), procSpec, ExitCode)
import Network.Wai (responseLBS, Application)
import Pipes.Core (Consumer, Producer)
import Pipes ((>->), runEffect, await, (>~))

type RepoAPI =
    Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> Get '[] ()
    :<|> Capture "userOrOrg" Text
        :> Capture "repo" Text
        :> "info" :> "refs"
        :> QueryParam "service" Text
        :> Raw


gitRecievePack = procSpec "git-receive-pack" []

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
        ((stdIn, stdOut), pid) <- liftIO $ pipeInputOutput Inherit gitRecievePack
        liftIO $ print "process_id"
        respond (responseRawProducer (\incomingData outgoingData -> runEffect (incomingData >-> (stdIn >~ ignoreExitCode))) backupResponse)

ignoreExitCode :: Consumer ExitCode IO ()
ignoreExitCode = do
    x <- await
    return ()

repoServer :: Server RepoAPI
repoServer =
    showHead
    :<|> initiateProtocol
