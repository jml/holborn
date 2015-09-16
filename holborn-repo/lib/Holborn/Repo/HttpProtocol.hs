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

import Servant ((:>), (:<|>)(..), Get, Post, Capture, QueryParam, Proxy(..), ServantErr, Server, OctetStream, Raw)
import Control.Monad.Trans.Either (EitherT)
import Pipes.Wai (responseProducer, Flush(..))
import Network.HTTP.Types.Status (ok200)
import Network.Wai (responseLBS, Application, responseStream)
import Pipes.Core (Consumer, Producer, Pipe)
import Pipes ((>->), runEffect, await, yield, (>~))
import Pipes.Shell (pipeCmd, producerCmd, runShell, (>?>))
import Pipes.Safe (SafeT)
import Blaze.ByteString.Builder (Builder, fromByteString)

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

repoServer :: Server RepoAPI
repoServer =
    showHead
    :<|> initiateProtocol

showHead :: Text -> Text -> EitherT ServantErr IO ()
showHead userOrOrg repo = return ()

backupResponse = responseLBS ok200 [] "problem?"

initiateProtocol :: Text -> Text -> Maybe Text -> Server Raw
initiateProtocol userOrOrg repo service =
    localrespond
  where
    localrespond :: Application
    localrespond req respond = do
        liftIO $ print userOrOrg
        liftIO $ print repo
        liftIO $ print service
        case service of
         Nothing -> respond backupResponse
         Just "git-upload-pack" -> respond $ responseStream ok200 gitHeaders (gitPack "git-upload-pack")

    gitPack :: String -> (Builder -> IO ()) -> IO () -> IO ()
    gitPack command moreData flush =
        runShell $ (banner >> (producerCmd ("git-upload-pack --stateless-rpc --advertise-refs /tmp/g0/") >-> filterStdErr) >> footer) >-> runChunks
      where
        runChunks :: Consumer ByteString (SafeT IO) ()
        runChunks = do
            chunk <- await
            liftIO $ moreData (fromByteString chunk)
            runChunks

    gitHeaders =
        [ ("Content-Type", "application/x-git-upload-pack-advertisement")
        , ("Pragma", "no-cache")
        , ("Server", "git")
        , ("Cache-Control", "no-cache, max-age=0, must-revalidate")
        ]

    banner = do
        yield "001e# service=git-upload-pack\n"
        yield "0000"
    footer = do
        yield "0000\n"

    filterStdErr :: Pipe (Either ByteString ByteString) ByteString (SafeT IO) ()
    filterStdErr = do
        x <- await
        case x of
            Left err -> terror (decodeUtf8 err)
            Right data_ -> do
                yield data_
                filterStdErr
