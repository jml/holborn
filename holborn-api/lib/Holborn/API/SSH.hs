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

import HolbornPrelude

import GHC.Generics (Generic)
import qualified Data.Text as Text
import Data.ByteString.Lazy (fromChunks, toStrict)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object, encode)
import Servant ((:>), (:<|>)(..), Capture, Get, Post, ReqBody, JSON, MimeRender(..), PlainText, Server, enter)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (AppConf(..))
import Holborn.API.Internal (APIHandler, JSONCodeableError(..), getConfig, logDebug, query, toServantHandler)
import Holborn.API.Types (Username)
import Holborn.JSON.SSHRepoCommunication ( RepoCall(..)
                                         , KeyType(..)
                                         , SSHCommandLine(..)
                                         , SSHKey
                                         , RepoAccess(..)
                                         , unparseSSHKey
                                         )
import Holborn.JSON.RepoMeta (RepoId)


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
server conf = enter (toServantHandler conf) $ checkKey
    :<|> checkRepoAccess
    :<|> listKeys


-- None of the SSH handlers can return any valid error. Instead, they do a
-- weird thing where always return success responses (i.e. 200 response code),
-- but some success responses indicate failure.
--
-- This is because the primary client of this API is a fork of OpenSSH which
-- we are trying to keep very simple.
--
-- Rather than have this fork understand complicated responses, we just give
-- it a shell command that produces the effect that we desire.

-- | Unconstructable data type that represents the error that we can never
-- ever raise. Fills in a placeholder in the APIHandler type signature.
data SSHError

instance JSONCodeableError SSHError where
  toJSON _ = (500, object [ "message" .= ("unexpected SSH error" :: Text) ])


-- | Wrapper around APIHandler to indicate that we're never using it to return
-- errors.
--
-- TODO: Probably a more clever thing would be to separate the config/io stuff
-- from the servanterr translation stuff. jml is not that clever yet.
type SSHHandler a = APIHandler SSHError a


newtype SSHKeys = SSHKeys { unSSHKeys :: [SSHKey] } deriving (ToJSON, Show)

instance MimeRender PlainText SSHKeys where
  -- | Turn a list of SSH keys into a line-separated plaintext dump that would
  -- serve as an authorized_keys file.
  mimeRender _ = fromChunks . map ((<> "\n") . unparseSSHKey) . unSSHKeys


-- | Implementation
checkKey :: CheckKeyRequest -> SSHHandler CheckKeyResponse
checkKey request@CheckKeyRequest{..} = do
    logDebug ("checkKey" :: Text, request)
    let comparison_pubkey = case key_type of
            RSA -> "ssh-rsa " <> key
            DSA -> "ssh-dsa " <> key
    logDebug ("actual db check" :: String, comparison_pubkey)
    rows <- query [sql|
                   select pk.id, pk.verified
                   from "public_key" as pk  where comparison_pubkey = ?
               |] (Only comparison_pubkey)
    logDebug rows

    return $ case rows of
       [(keyId, True)] -> CheckKeyResponse (Just keyId)
       -- PUPPY: We currently don't have any support for verifying SSH keys.
       -- To help us test our server in the mean-time, just pretend that every
       -- key is verified. In the future, non-verified keys will not be
       -- allowed to access the system.
       [(keyId, False)] -> CheckKeyResponse (Just keyId)
       _ -> terror "TODO return error for checkKey"


-- | List all the verified SSH keys for a user.
listKeys :: Username -> SSHHandler SSHKeys
listKeys username = do
    logDebug ("listing keys for" :: Text, username)
    keys <- query keysQuery (Only username)
    return $ SSHKeys keys
  where
    keysQuery =
      [sql|select submitted_pubkey
           from public_key
           join "user" as u on u.id = owner_id and u.username = ?
           where public_key.verified
          |]


-- | Different ways an SSH request can fail.
data SSHAccessError
  = NoSSHKey
  | MultipleSSHKeys
  | ReadOnlyKey KeyId
  | UnverifiedKey
  deriving (Eq, Show)


-- | Determine whether the user identified by their SSH key can access a repo.
checkRepoAccess' :: CheckRepoAccessRequest -> SSHHandler (Either SSHAccessError RepoCall)
checkRepoAccess' CheckRepoAccessRequest{key_id, command} = do
    let (owner, repo) = (_owner command, _sshCommandLineRepo command)

    rows <- query [sql|
                   select id, pk.readonly, pk.verified
                   from "public_key" as pk where id = ?
               |] (Only key_id)
    logDebug (key_id, command, rows)

    -- TODO - the following is just a placeholder query so we can get
    -- a repoId. It works but needs error handling (return e.g. 404
    -- when repo wasn't found).
    [(_ :: String, repoId :: RepoId)] <- query [sql|
               select 'org', id from "org" where orgname = ? and name = ?
               UNION
               select 'user',  id from "user" where username = ? and name = ?
               |] (owner, repo, owner, repo)

    case (command, rows) of
        (_, []) -> return $ Left NoSSHKey
        (_, _:_:_) -> return $ Left MultipleSSHKeys
        (GitReceivePack _ _, [(_, False, True)]) -> return $ Right $ WritableRepoCall command repoId
        (GitReceivePack _ _, [(keyId, True, True)]) -> return $ Left $ ReadOnlyKey keyId
        (GitUploadPack _ _, [(_, _, True)]) -> return $ Right $ WritableRepoCall command repoId
        (_, [(_, _, False)]) -> return $ Left UnverifiedKey


-- | Either route a git request to the correct repo, or give an error saying
-- why not.
routeRepoRequest :: CheckRepoAccessRequest -> SSHHandler (Either SSHAccessError RepoAccess)
routeRepoRequest request = do
    AppConf{rawRepoHostname, rawRepoPort} <- getConfig
    fmap (AccessGranted rawRepoHostname rawRepoPort) <$> checkRepoAccess' request


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
renderAccess :: Either SSHAccessError RepoAccess -> Text
renderAccess (Left err) = ">&2 echo '" <> toErrorMessage err <> "' && exit 1"
  where
    toErrorMessage NoSSHKey = "No SSH key"
    toErrorMessage MultipleSSHKeys = "Multiple SSH keys"
    toErrorMessage (ReadOnlyKey keyId) = "SSH key with id " <> show (keyId :: Int) <> " is readonly"
    toErrorMessage UnverifiedKey = "SSH key not verified"
renderAccess (Right (AccessGranted hostname port repoCall)) =
  concat ["(echo -n "
         , shellEncode repoCall
         , " && cat) | nc "
         , hostname
         , " "
         , fromShow port
         ]


checkRepoAccess :: CheckRepoAccessRequest -> SSHHandler CheckRepoAccessResponse
checkRepoAccess request = do
    route <- routeRepoRequest request
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
