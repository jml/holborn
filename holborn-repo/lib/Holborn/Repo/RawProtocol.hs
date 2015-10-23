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
import           System.Exit (ExitCode)

gitUploadPack :: Producer ByteString IO () -> Consumer ByteString IO () -> IO (ExitCode, ())
gitUploadPack from to =
    execute ((proc "git-upload-pack" ["/tmp/hello"]) { std_in = CreatePipe, std_out = CreatePipe } ) doRun
  where
    doRun stdin stdout stderr = do
       _ <- forkIO (runEffect (from >-> toOutput stdin))
       _ <- forkIO (runEffect (fromInput stdout >-> to))
       runEffect (fromInput stderr >-> P.print)
       return ()

data RepoCall = RepoCall { name :: Text } deriving (Show, Generic)
instance FromJSON RepoCall

getRepoParser :: MonadIO m => PP.Parser ByteString m (Maybe RepoCall)
getRepoParser = do
    repoCall <- decode
    return $ case repoCall of
      Just (Right x) -> (Just x)
      _ -> Nothing

accept :: (Socket, SockAddr) -> IO ()
accept (sock, _) = do
    let from = fromSocket sock 4096
    let to = toSocket sock
    (r, fromRest) <- runStateT getRepoParser from
    liftIO $ print r
    _ <- gitUploadPack fromRest to
    return ()

serveRaw :: (PS.MonadSafe m) => m ()
serveRaw = serve HostAny "8082" accept
