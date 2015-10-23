{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
-- | When connecting via SSH we don't have the luxury of the HTTP
-- protocol so we need to be able to run git-{receive,upload}-pack
-- directly with stdin and stdout connected to a network socket.
--
-- To avoid having many open ports we have just one extra port and a
-- really dumb line terminated protocol to select the correct repo.

module Holborn.Repo.RawProtocol
       ( serveRaw
       ) where

import           BasicPrelude

import           Control.Concurrent (forkIO)
import           Control.Monad.State.Strict (runStateT)
import           Data.Aeson (FromJSON)
import           GHC.Generics (Generic)
import           Holborn.Repo.GabrielProcess (proc, execute, toOutput, fromInput, std_in, std_out, StdStream(..))
import           Network.Socket (Socket, SockAddr)
import           Pipes ((>->), runEffect)
import           Pipes.Aeson (decode)
import           Pipes.Core (Consumer, Producer)
import           Pipes.Network.TCP (HostPreference(..))
import           Pipes.Network.TCP.Safe (serve, fromSocket, toSocket)
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as PS
import           System.Exit (ExitCode(..))

gitPack :: String -> Text -> Text -> Producer ByteString IO () -> Consumer ByteString IO () -> IO (ExitCode, ())
gitPack packCommand org repo from to =
    execute ((proc packCommand ["/tmp/hello"]) { std_in = CreatePipe, std_out = CreatePipe } ) doRun
  where
    doRun stdin stdout stderr = do
       _ <- forkIO (runEffect (from >-> toOutput stdin))
       _ <- forkIO (runEffect (fromInput stdout >-> to))
       runEffect (fromInput stderr >-> P.print)
       return ()

gitUploadPack :: Text -> Text -> Producer ByteString IO () -> Consumer ByteString IO () -> IO (ExitCode, ())
gitUploadPack = gitPack "git-upload-pack"

gitReceivePack :: Text -> Text -> Producer ByteString IO () -> Consumer ByteString IO () -> IO (ExitCode, ())
gitReceivePack = gitPack "git-receive-pack"

data RepoCall = RepoCall { command :: Text, org :: Text, repo :: Text } deriving (Show, Generic)
instance FromJSON RepoCall

-- | The openssh thingy needs to tell us which repository we're
-- talking about before it establishes the bidirectional pipe for the
-- git pack commands.
--
-- The header format is just a single serialized JSON string of type
-- RepoCall. The closing `}` will be parsed, everything that follows
-- is piped directly to git.
getRepoParser :: MonadIO m => PP.Parser ByteString m (Maybe RepoCall)
getRepoParser = do
    repoCall <- decode
    return $ case repoCall of
      Just (Right x) -> (Just x)
      _ -> Nothing

-- | Accept connection, parse the exact repository, then dispatch to
-- git.
accept :: (Socket, SockAddr) -> IO ()
accept (sock, _) = do
    let from = fromSocket sock 4096
    let to = toSocket sock
    (r, fromRest) <- runStateT getRepoParser from
    liftIO $ print r
    _ <- case r of
        Just (RepoCall "git-upload-pack" org repo) ->
            gitUploadPack org repo fromRest to
        Just (RepoCall "git-receive-pack" org repo) ->
            gitReceivePack org repo fromRest to
        _ -> terror "if the data is properly cleaned this doesn't happen"
    return ()

serveRaw :: (PS.MonadSafe m) => m ()
serveRaw = serve HostAny "8082" accept
