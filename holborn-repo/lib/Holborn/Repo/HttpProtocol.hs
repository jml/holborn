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
import Text.Printf (printf)

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
    :<|> smartHandshake

showHead :: Text -> Text -> EitherT ServantErr IO ()
showHead userOrOrg repo = return ()

backupResponse = responseLBS ok200 [] "problem?"

-- | Render in pkg format (4 byte hex prefix for total line length including header)
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
        case service of
            Nothing -> respond backupResponse
            Just "git-upload-pack" ->
                respond $ responseStream ok200 (gitHeaders "git-upload-pack") (gitPack "git-upload-pack")
            Just "git-receive-pack" ->
                respond $ responseStream ok200 (gitHeaders "git-receive-pack") (gitPack "git-receive-pack")

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
        yield "0000\n"

    filterStdErr :: Pipe (Either ByteString ByteString) ByteString (SafeT IO) ()
    filterStdErr = do
        x <- await
        case x of
            -- TODO send errors to journald && maybe measure
            Left err -> terror (decodeUtf8 err)
            Right data_ -> do
                yield data_
                filterStdErr
