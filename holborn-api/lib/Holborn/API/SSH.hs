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
  , api
  , server
    -- | The following are library functions used by command-line tools.
    -- Perhaps they should be moved to another module.
  , KeyId
    -- | Need these for API requests
  , CheckKeyRequest(..)
  , CheckRepoAccessRequest(..)
    -- | Need these for authentication requests
  , SSHKeys(unSSHKeys)
  ) where

import HolbornPrelude

import GHC.Generics (Generic)
import Data.ByteString.Lazy (fromChunks)
import Data.Proxy (Proxy(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object)
import Servant ((:>), (:<|>)(..), Capture, Get, Post, ReqBody, JSON, MimeRender(..), PlainText, Server, enter)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (AppConf(..))
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , APIError(InsufficientPermissions)
  , RepoAccess(..)
  , logDebug
  , query
  , routeRepoRequest
  , throwAPIError
  , toServantHandler
  )
import Holborn.API.Types (Username)
import Holborn.JSON.SSHRepoCommunication
  ( KeyType(..)
  , GitCommand(..)
  , SSHCommandLine(..)
  , SSHKey
  , unparseKeyType
  , unparseSSHKey
  )


-- | Main internal API (only used by our openssh version ATM).
type API =
    "ssh" :> (
       "access-repo"
       :> ReqBody '[JSON] CheckRepoAccessRequest
       :> Post '[JSON] RepoAccess
       :<|> Capture "user" Username
       :> "keys"
       :> Get '[JSON, PlainText] SSHKeys
       :<|> "authorized-keys"
       :> ReqBody '[JSON] CheckKeyRequest
       :> Post '[JSON, PlainText] SSHKeys
       )


api :: Proxy API
api = Proxy


server :: AppConf -> Server API
server conf = enter (toServantHandler conf) $
    accessRepo
    :<|> listKeys
    :<|> authorizedKeys


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


newtype SSHKeys = SSHKeys { unSSHKeys :: [(KeyId, SSHKey)] } deriving (ToJSON, FromJSON, Show)

instance MimeRender PlainText SSHKeys where
  -- | Turn a list of SSH keys into a line-separated plaintext dump that would
  -- serve as an authorized_keys file.
  mimeRender _ = fromChunks . map ((<> "\n") . unparseSSHKey) . map snd . unSSHKeys


-- | List all the verified SSH keys for a user.
listKeys :: Username -> SSHHandler SSHKeys
listKeys username = do
    logDebug ("listing keys for" :: Text, username)
    keys <- query keysQuery (Only username)
    return $ SSHKeys keys
  where
    -- PUPPY: We are including unverfied keys in this "authorized keys" list,
    -- which is a potential security escalation vector.
    keysQuery =
      [sql|select public_key.id, submitted_pubkey
           from public_key
           join "user" as u on u.id = owner_id and u.username = ?
          |]


-- | Get all authorized keys that match the supplied keys.
authorizedKeys :: CheckKeyRequest -> SSHHandler SSHKeys
authorizedKeys CheckKeyRequest{..} = do
  let comparison_pubkey = unparseKeyType key_type <> " " <> key
  -- PUPPY: We are including unverfied keys in this "authorized keys" list,
  -- which is a potential security escalation vector.
  rows <- query [sql|select pk.id, pk.submitted_pubkey
                     from "public_key" as pk
                     where comparison_pubkey = ?
                     |] (Only comparison_pubkey)
  return $ SSHKeys rows


-- | Different ways an SSH request can fail.
data SSHAccessError
  = NoSSHKey
  | MultipleSSHKeys
  | ReadOnlyKey KeyId
  | UnverifiedKey
  deriving (Eq, Show)


-- | Determine whether the user identified by their SSH key can access a repo.
checkRepoAccess' :: CheckRepoAccessRequest -> SSHHandler (Either SSHAccessError RepoAccess)
checkRepoAccess' CheckRepoAccessRequest{key_id, command} = do
    rows <- query [sql|
                   select id, pk.readonly, pk.verified
                   from "public_key" as pk where id = ?
               |] (Only key_id)
    logDebug (key_id, command, rows)

    let SSHCommandLine command' owner name = command
    case rows of
      []                        -> pure $ Left NoSSHKey
      _:_:_                     -> pure $ Left MultipleSSHKeys
      [(_, _, False)]           -> pure $ Left UnverifiedKey
      [(keyId, readOnly, True)] ->
        case (command', readOnly) of
          (GitReceivePack, True)  -> pure $ Left $ ReadOnlyKey keyId
          _                       -> Right <$> routeRepoRequest command' owner name


accessRepo :: CheckRepoAccessRequest -> SSHHandler RepoAccess
accessRepo request = do
  route <- checkRepoAccess' request
  case route of
      Left _ -> throwAPIError InsufficientPermissions
      Right route' -> return route'


data CheckKeyRequest = CheckKeyRequest
    { key :: Text
    , key_type :: KeyType
    } deriving (Show, Generic, Eq, Ord)
instance FromJSON CheckKeyRequest
instance ToJSON CheckKeyRequest

type KeyId = Int


data CheckRepoAccessRequest = CheckRepoAccessRequest
    { key_id :: KeyId
    , command :: SSHCommandLine
    } deriving (Show, Generic)

instance FromJSON CheckRepoAccessRequest
instance ToJSON CheckRepoAccessRequest
