{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Internal API for e.g. checking whether a user is authorized to
-- access a repository.
--
-- This API is highly specific to our current openssh implementation
-- and has absolutely no stability guarantees.

module Holborn.API.SSH
       ( API
       , server
       ) where

import BasicPrelude

import GHC.Generics (Generic)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString.Lazy (fromChunks, toStrict)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object, encode)
import Servant ((:>), (:<|>)(..), Capture, Get, Post, ReqBody, JSON, MimeRender(..), PlainText, ServantErr, Server)
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (AppConf(..))
import Holborn.API.Types (Username)
import Holborn.JSON.SSHRepoCommunication ( RepoCall(..)
                                         , KeyType(..)
                                         , SSHCommandLine(..)
                                         , SSHKey
                                         , unparseSSHKey
                                         )
import Network.Wai.Handler.Warp (Port)
import qualified Holborn.Logging as Log


-- | Main internal API (only used by our openssh version ATM).
type API =
    "ssh" :> (
       "check-key"
       :> ReqBody '[JSON] CheckKeyRequest
       :> Post '[JSON] CheckKeyResponse
       :<|> "check-repo-access"
       :> ReqBody '[JSON] CheckRepoAccessRequest
       :> Post '[JSON] CheckRepoAccessResponse
       :<|> Capture "user" Username
       :> "keys"
       :> Get '[JSON, PlainText] SSHKeys
       )


server :: AppConf -> Server API
server conf = checkKey conf
    :<|> checkRepoAccess conf
    :<|> listKeys conf


newtype SSHKeys = SSHKeys { unSSHKeys :: [SSHKey] } deriving (ToJSON, Show)

instance MimeRender PlainText SSHKeys where
  -- | Turn a list of SSH keys into a line-separated plaintext dump that would
  -- serve as an authorized_keys file.
  mimeRender _ = fromChunks . map ((<> "\n") . unparseSSHKey) . unSSHKeys


-- | Implementation
checkKey :: AppConf -> CheckKeyRequest -> ExceptT ServantErr IO CheckKeyResponse
checkKey AppConf{conn} request@CheckKeyRequest{..} = do
    Log.debug ("checkKey" :: Text, request)
    let comparison_pubkey = case key_type of
            RSA -> "ssh-rsa " <> key
            DSA -> "ssh-dsa " <> key
    Log.debug ("actual db check" :: String, comparison_pubkey)
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


-- | List all the verified SSH keys for a user.
listKeys :: AppConf -> Username -> ExceptT ServantErr IO SSHKeys
listKeys AppConf{conn} username = do
    Log.debug ("listing keys for" :: Text, username)
    keys <- liftIO $ query conn keysQuery (Only username)
    return $ SSHKeys keys
  where
    keysQuery =
      [sql|select submitted_pubkey
           from public_key
           join "user" as u on u.id = owner_id and u.username = ?
           where public_key.verified
          |]


-- | Emit the Holborn side of the SSH command
accessGranted :: Text -> Port -> SSHCommandLine -> Text
accessGranted hostname port commandLine =
  concat ["(echo -n '"
         , decodeUtf8 (toStrict (encode repoCall))
         , "' && cat) | nc "
         , hostname
         , " "
         , fromShow port
         ]
  where
    repoCall =
      case commandLine of
        GitReceivePack org repo -> WritableRepoCall "git-receive-pack" org repo
        GitUploadPack org repo -> WritableRepoCall "git-upload-pack" org repo


checkRepoAccess :: AppConf -> CheckRepoAccessRequest -> ExceptT ServantErr IO CheckRepoAccessResponse
checkRepoAccess AppConf{conn, rawRepoHostname, rawRepoPort} request = do
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
        (GitReceivePack _ _, [(_, False, True)]) -> accessGranted rawRepoHostname rawRepoPort cmd
        (GitReceivePack _ _, [(keyId, True, True)]) ->
            reportError ("SSH key with id " <> show (keyId :: Int) <> " is readonly")
        (GitUploadPack _ _, [(_, _, True)]) -> accessGranted rawRepoHostname rawRepoPort cmd
        (_, [(_, _, False)]) -> reportError "SSH key not verified"


-- Serialization bla bla
data CheckKeyRequest = CheckKeyRequest
    { key :: Text
    , key_type :: KeyType
    } deriving (Show, Generic, Eq, Ord)
instance FromJSON CheckKeyRequest


type KeyId = Int


data CheckKeyResponse = CheckKeyResponse
    { _key_id :: Maybe KeyId -- TODO might be more useful to return Either with error message?
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
      _target :: Maybe Text -- e.g. "nc 127.0.0.1:8080"
    } deriving (Show)

instance ToJSON CheckRepoAccessResponse where
    toJSON (CheckRepoAccessResponse (Just target)) =
        object [ "allowed" .= True
               , "target"  .= target
               ]
    toJSON (CheckRepoAccessResponse Nothing) =
        object [ "allowed" .= False
               ]
