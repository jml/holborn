{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Internal API for e.g. checking whether a user is authorized to
-- access a repository.
--
-- This API is highly specific to our current openssh implementation
-- and has absolutely no stability guarantees.

module Holborn.API.Internal
       ( API
       , server
       ) where

import BasicPrelude

import GHC.Generics (Generic)
import Control.Error (rightZ)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object, withText)
import Servant ((:>), (:<|>)(..), Post, ReqBody, JSON, ServantErr, Server)
import qualified Data.Attoparsec.Text as AT
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Types (AppConf(..), KeyType(..))
import qualified Holborn.Logging as Log


-- | Main internal API (only used by our openssh version ATM).
type API =
    "internal" :> "ssh" :> (
       "check-key"
       :> ReqBody '[JSON] CheckKeyRequest
       :> Post '[JSON] CheckKeyResponse
       :<|> "check-repo-access"
       :> ReqBody '[JSON] CheckRepoAccessRequest
       :> Post '[JSON] CheckRepoAccessResponse)


-- | Implementation
checkKey :: AppConf -> CheckKeyRequest -> ExceptT ServantErr IO CheckKeyResponse
checkKey AppConf{conn} request = do
    Log.debug ("checkKey" :: Text, request)
    let comparison_pubkey = case key_type request of
            RSA -> "ssh-rsa " <> key request
            DSA -> "ssh-dsa " <> key request
    rows <- liftIO $ query conn [sql|
                   select pk.id, pk.verified
                   from "public_key" as pk  where comparison_pubkey = ?
               |] (Only comparison_pubkey)
    Log.debug rows

    return $ case rows of
       [(keyId, True)] -> CheckKeyResponse (Just keyId)
       -- PUPPY: We currently don't have any support for verifying SSH keys.
       -- To help us test our server in the mean-time, just pretend that every
       -- key is verified. In the future, non-verified keys will not be
       -- allowed to access the system.
       [(keyId, False)] -> CheckKeyResponse (Just keyId)
       _ -> terror "TODO return error for checkKey"


data SSHCommandLine =
      GitReceivePack { orgOrUser :: Text, repo :: Text }
    | GitUploadPack { orgOrUser :: Text, repo :: Text }
    deriving Show


-- | Emit the Holborn side of the SSH command
accessGranted :: SSHCommandLine -> Text
accessGranted (GitReceivePack org repo) =
  concat ["(echo -n '{\"command\": \"git-receive-pack\", \"org\": \""
         , org
         , "\", \"repo\": \""
         , repo
         ,"\"}' && cat) | nc 127.0.0.1 8081"
         ]
accessGranted (GitUploadPack org repo) =
  concat ["(echo -n '{\"command\": \"git-upload-pack\", \"org\": \""
         , org
         , "\", \"repo\": \""
         , repo
         ,"\"}' && cat) | nc 127.0.0.1 8081"
         ]


-- There are two acceptable commands:
--   "git-upload-pack '/org/hello'"
--   "git-receive-pack '/org/hello'"
-- For all other commands we can send back futurama quotes.
--
-- PUPPY - this is a security sensitive piece (gatekeeper for a
-- remote ssh trying to run random commands) and as such it needs
-- quickchecking!
parseSSHCommand :: AT.Parser SSHCommandLine
parseSSHCommand =
    upload <|> receive
  where
    upload = do
        void $ AT.string "git-upload-pack '"
        uncurry GitUploadPack <$> repoPath
    receive = do
        void $ AT.string "git-receive-pack '"
        uncurry GitReceivePack <$> repoPath
    repoPath = do
        AT.skipWhile (== '/') -- skip optional leading /
        org <- AT.takeWhile1 (/= '/')
        void $ AT.char '/'
        user <- AT.takeWhile1 (/= '\'')
        void $ AT.char '\''
        AT.endOfInput
        return (org, user)


instance FromJSON SSHCommandLine where
  parseJSON = withText "SSH command must be text" (rightZ . AT.parseOnly parseSSHCommand)


checkRepoAccess :: AppConf -> CheckRepoAccessRequest -> ExceptT ServantErr IO CheckRepoAccessResponse
checkRepoAccess AppConf{conn} request = do
    Log.debug request
    rows <- liftIO $ query conn [sql|
                   select id, pk.readonly, pk.verified
                   from "public_key" as pk where id = ?
               |] (Only (key_id request))

    -- OpenSSH runs the command we send in bash, so we can use common
    -- shell muckery to first send the metadata and then do a
    -- bidirectional pipe.
    let cmd = command request
    Log.debug (cmd, rows)
    return . CheckRepoAccessResponse . Just $ determineAccess cmd rows
  where
    reportError err = ">&2 echo '" <> err <> "' && exit 1"

    determineAccess cmd rows =
      case (cmd, rows) of
        (_, []) -> reportError "No SSH key"
        (_, _:_:_) -> reportError "Multiple SSH keys"
        (GitReceivePack _ _, [(_, False, True)]) -> accessGranted cmd
        (GitReceivePack _ _, [(keyId, True, True)]) ->
            reportError ("SSH key with id " <> show (keyId :: Int) <> " is readonly")
        (GitUploadPack _ _, [(_, _, True)]) -> accessGranted cmd
        (_, [(_, _, False)]) -> reportError "SSH key not verified"


server :: AppConf -> Server API
server conf = checkKey conf
    :<|> checkRepoAccess conf


-- Serialization bla bla
data CheckKeyRequest = CheckKeyRequest
    { key :: Text
    , key_type :: KeyType
    } deriving (Show, Generic, Eq, Ord)
instance FromJSON CheckKeyRequest


type KeyId = Int


data CheckKeyResponse = CheckKeyResponse
    { key_id_ :: Maybe KeyId -- TODO might be more useful to return Either with error message?
    } deriving (Show, Generic)

instance ToJSON CheckKeyResponse where
    toJSON (CheckKeyResponse (Just key_id_)) =
        object [ "allowed"  .= True
               , "key_id" .= key_id_
               ]
    toJSON (CheckKeyResponse Nothing) =
        object [ "allowed"  .= False
               ]


data CheckRepoAccessRequest = CheckRepoAccessRequest
    { key_id :: KeyId
    , command :: SSHCommandLine
    } deriving (Show, Generic)
instance FromJSON CheckRepoAccessRequest


data CheckRepoAccessResponse = CheckRepoAccessResponse
    { -- | 'Nothing' means no access allowed. Otherwise, the command that the SSH server should run in a shell.
      target :: Maybe Text -- E.g. "nc 127.0.0.1:8080"
    } deriving (Show)

instance ToJSON CheckRepoAccessResponse where
    toJSON (CheckRepoAccessResponse (Just target)) =
        object [ "allowed" .= True
               , "target"  .= target
               ]
    toJSON (CheckRepoAccessResponse Nothing) =
        object [ "allowed" .= False
               ]
