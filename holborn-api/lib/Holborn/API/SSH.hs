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
       -- | Exported for testing
       , shellEncode
       , shellQuote
       ) where

import BasicPrelude

import GHC.Generics (Generic)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Data.Text as Text
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
                                         , RepoAccess(..)
                                         , unparseSSHKey
                                         )
import Holborn.JSON.RepoMeta (RepoId)

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


-- | Determine whether the user identified by their SSH key can access a repo.
checkRepoAccess' :: AppConf -> CheckRepoAccessRequest -> ExceptT Text IO RepoCall
checkRepoAccess' AppConf{conn} CheckRepoAccessRequest{key_id, command} = do
    let (owner, repo) = case command of
          GitReceivePack _owner _sshCommandLineRepo -> (_owner, _sshCommandLineRepo)
          GitUploadPack _owner _sshCommandLineRepo -> (_owner, _sshCommandLineRepo)

    rows <- liftIO $ query conn [sql|
                   select id, pk.readonly, pk.verified
                   from "public_key" as pk where id = ?
               |] (Only key_id)
    Log.debug (key_id, command, rows)

    [(_ :: String, repoId :: RepoId)] <- liftIO $ query conn [sql|
               select 'org', id from "org" where orgname = ? and name = ?
               UNION
               select 'user',  id from "user" where username = ? and name = ?
               |] (owner, repo, owner, repo)

    case (command, rows) of
        (_, []) -> throwE "No SSH key"
        (_, _:_:_) -> throwE "Multiple SSH keys"
        (GitReceivePack _ _, [(_, False, True)]) -> return $ WritableRepoCall command repoId
        (GitReceivePack _ _, [(keyId, True, True)]) ->
            throwE ("SSH key with id " <> show (keyId :: Int) <> " is readonly")
        (GitUploadPack _ _, [(_, _, True)]) -> return $ WritableRepoCall command repoId
        (_, [(_, _, False)]) -> throwE "SSH key not verified"


-- | Either route a git request to the correct repo, or give an error saying
-- why not.
routeRepoRequest :: AppConf -> CheckRepoAccessRequest -> ExceptT Text IO RepoAccess
routeRepoRequest conf@AppConf{rawRepoHostname, rawRepoPort} request =
    AccessGranted rawRepoHostname rawRepoPort <$> checkRepoAccess' conf request


-- | Encode a JSON object so that it can be echoed on the shell.
--
-- We want this because our customized SSH server operates by executing a
-- command returned from this API server in the shell.
shellEncode :: (ToJSON a) => a -> Text
shellEncode = shellQuote . decodeUtf8 . toStrict . encode


-- | Surround 'str' in single quotes, and wrap every literal single quote
-- in a double quotes that are outside the single quotes.
--
-- e.g. the literal:
--     foo 'bar' baz
--
-- becomes:
--     'foo '"'"'bar'"'"' baz'
--
-- which would be echoed as:
--     foo 'bar' baz
shellQuote :: Text -> Text
shellQuote str = "'" <> escape str <> "'"
  where
    escape = escapeBackslashes . escapeSingleQuotes
    escapeSingleQuotes = Text.replace "'" "'\"'\"'"
    escapeBackslashes = Text.replace "\\" "\\\\"


-- | Emit the Holborn side of the SSH command
renderAccess :: Either Text RepoAccess -> Text
renderAccess (Left err) = ">&2 echo '" <> err <> "' && exit 1"
renderAccess (Right (AccessGranted hostname port repoCall)) =
  concat ["(echo -n "
         , shellEncode repoCall
         , " && cat) | nc "
         , hostname
         , " "
         , fromShow port
         ]


checkRepoAccess :: AppConf -> CheckRepoAccessRequest -> ExceptT ServantErr IO CheckRepoAccessResponse
checkRepoAccess conf request = do
    route <- liftIO $ runExceptT (routeRepoRequest conf request)
    return . CheckRepoAccessResponse . Just . renderAccess $ route


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
