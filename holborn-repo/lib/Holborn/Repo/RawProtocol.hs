{-# LANGUAGE RankNTypes #-}


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

import           Control.Monad.State.Strict (runStateT)
import           Network.Socket (Socket, SockAddr)
import           Pipes.Core (Consumer, Producer)
import           Pipes.Network.TCP (HostPreference(..))
import           Pipes.Network.TCP.Safe (serve, fromSocket, toSocket)
import qualified Pipes.Parse as PP
import           Pipes.Safe (runSafeT)
import           Pipes.Aeson (decode)
import           Holborn.Repo.Config (Config, buildRepoPath)
import           Holborn.Repo.Process (streamIO, proc)
import qualified Holborn.Logging as Log
import           Holborn.JSON.SSHRepoCommunication (RepoCall(..))


gitPack :: String -> Config -> Text -> Text -> Producer ByteString IO () -> Consumer ByteString IO () -> IO ()
gitPack packCommand config org repo from to = do
    void $ streamIO (proc packCommand [buildRepoPath config org repo]) from to
    return ()

gitUploadPack :: Config -> Text -> Text -> Producer ByteString IO () -> Consumer ByteString IO () -> IO ()
gitUploadPack = gitPack "git-upload-pack"

gitReceivePack :: Config -> Text -> Text -> Producer ByteString IO () -> Consumer ByteString IO () -> IO ()
gitReceivePack = gitPack "git-receive-pack"


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
      Just (Right x) -> Just x
      _ -> Nothing

-- | Accept connection, parse the exact repository, then dispatch to
-- git.
accept :: Config -> (Socket, SockAddr) -> IO ()
accept config (sock, _) = do
    let from = fromSocket sock 4096
    let to = toSocket sock
    (header, fromRest) <- runStateT getRepoParser from
    Log.debug header
    void $ case header of
        Just (RepoCall "git-upload-pack" org repo) ->
            gitUploadPack config org repo fromRest to
        Just (RepoCall "git-receive-pack" org repo) ->
            gitReceivePack config org repo fromRest to
        _ -> terror "if the data is properly cleaned this doesn't happen"
    return ()

serveRaw :: Config -> IO ()
serveRaw config = runSafeT (serve HostAny "8081" (accept config))
