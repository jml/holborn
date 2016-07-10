-- | Forced command for incoming SSH connections.
--
-- Responsible for interpreting the incoming command (stored as the
-- SSH_ORIGINAL_COMMAND environment variable) and either:
--
-- * rejecting it out of hand, because it is completely unwelcome
-- * translating it into a git repo request, which will be either:
--   * reading a git repo
--   * writing to a git repo
-- * processing the response, which will be one of:
--   * no such repo
--   * forbidden (so push to your own instead)
--   * forbidden (so sod off)
--   * here's where the repo to read from is
--   * here's where the repo to write to is
--
-- If we actually set up reading or writing to a repo, we thehn devolve into
-- merely shipping bytes back & forth.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import HolbornPrelude

import Control.Concurrent (forkIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Proxy (Proxy(..))
import qualified Env
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Pipes ((>->), runEffect)
import Pipes.Aeson.Unchecked (encode)
import qualified Pipes.ByteString as PBS
import Pipes.Network.TCP (SockAddr, Socket, connect, fromSocket, toSocket)
import Servant.API ((:>), Post, ReqBody, JSON)
import Servant.Client (ServantError, client, BaseUrl(..), parseBaseUrl)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import Holborn.API.SSH
  ( KeyId
  , CheckRepoAccessRequest(..)
  )
import Holborn.JSON.SSHRepoCommunication
  ( SSHCommandLine(..)
  , parseSSHCommand
  )
import Holborn.API.Internal (RepoAccess(..))


-- XXX: Unclear to jml why we are using keyID and not username.

-- | All the settings we need to query holborn-api for repository access.
data Config = Config
  { originalCommand :: Text
    -- ^ The command-line entered by the SSH client.
  , apiURL :: String
    -- ^ The URL of holborn-api
  , keyId :: KeyId
    -- ^ The ID of the SSH key used to connect to the server.
  } deriving (Show, Eq)


type API = "internal" :> "ssh" :> "access-repo"
           :> ReqBody '[JSON] CheckRepoAccessRequest
           :> Post '[JSON] RepoAccess


api :: Proxy API
api = Proxy

checkRepoAccess :: CheckRepoAccessRequest -> Manager -> BaseUrl -> ExceptT ServantError IO RepoAccess
checkRepoAccess = client api


-- | Extract configuration from the environment.
loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "holborn-connect-repo") $
  Config
  <$> Env.var Env.str
      "SSH_ORIGINAL_COMMAND" (Env.help "original command requested by user")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_API_URL" (Env.def "http://localhost:8002" <> Env.help "URL of the API server")
  <*> Env.var (Env.auto Env.<=< Env.nonempty)
      "HOLBORN_KEY_ID" (Env.help "ID of the key being used to access the repo")


-- | Call the Holborn API server to see if the given key ID is allowed to
-- access the repo.
checkPermissions :: BaseUrl -> Manager -> KeyId  -> SSHCommandLine -> IO (Either ServantError RepoAccess)
checkPermissions baseUrl manager keyId commandLine = do
  let request = CheckRepoAccessRequest keyId commandLine
  runExceptT $ checkRepoAccess request manager baseUrl


-- | Given 'RepoAccess' (more or less a capability to access a repo), connect
-- to a git process for that repository.
--
-- Starts a bidirectional pipe to the repo server where the repository
-- actually is, runs the correct git command, and then connects it to
-- stdin/stdout here.
execRepoCall :: RepoAccess -> IO ()
execRepoCall (AccessGranted hostname port call) = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  connect (textToString hostname) (fromShow port) pipeGit
  where
    pipeGit :: (Socket, SockAddr) -> IO ()
    pipeGit (socket, _) = do
      void $ forkIO $ runEffect $ (encode call >> PBS.stdin) >-> toSocket socket
      runEffect $ fromSocket socket 4096 >-> PBS.stdout


main :: IO ()
main = do
  Config{..} <- loadConfig
  baseUrl <- parseBaseUrl apiURL
  manager <- newManager defaultManagerSettings
  case parseSSHCommand originalCommand of
    Nothing -> printErr ("Invalid command, I'm just a git server: " <> originalCommand)
    Just sshCommand -> do
      repoCall <- checkPermissions baseUrl manager keyId sshCommand
      case repoCall of
        Left e -> print e
        Right call -> execRepoCall call
