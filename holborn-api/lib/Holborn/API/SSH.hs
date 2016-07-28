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
import Servant ((:>), (:<|>)(..), Post, ReqBody, JSON, MimeRender(..), PlainText, Server, enter)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , RepoAccess(..)
  , logDebug
  , query
  , routeRepoRequest
  , throwHandlerError
  , toServantHandler
  )
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
       :<|> "authorized-keys"
       :> ReqBody '[JSON] CheckKeyRequest
       :> Post '[JSON, PlainText] SSHKeys
       )


api :: Proxy API
api = Proxy


server :: Config -> Server API
server conf = enter (toServantHandler conf) $
    accessRepo :<|> authorizedKeys


-- | Different ways an SSH request can fail.
data SSHError
  = NoSSHKey
  | MultipleSSHKeys
  | ReadOnlyKey KeyId
  | UnverifiedKey
  deriving (Eq, Show)

instance JSONCodeableError SSHError where
  toJSON NoSSHKey = (401, object [ "message" .= ("Could not find SSH key" :: Text) ])
  toJSON MultipleSSHKeys = (500, object [ "message" .= ("Found multiple matching SSH keys" :: Text) ])
  toJSON (ReadOnlyKey keyId) = (403, object [ "message" .= ("Cannot push to repository with read-only key" :: Text)
                                            , "keyId" .= keyId
                                            ])
  toJSON UnverifiedKey = (403, object [ "message" .= ("Cannot access repositories with key that has not been verified" :: Text)])


-- | Wrapper around APIHandler for SSH endpoints.
type SSHHandler a = APIHandler SSHError a

newtype SSHKeys = SSHKeys { unSSHKeys :: [(KeyId, SSHKey)] } deriving (ToJSON, FromJSON, Show)

instance MimeRender PlainText SSHKeys where
  -- | Turn a list of SSH keys into a line-separated plaintext dump that would
  -- serve as an authorized_keys file.
  mimeRender _ = fromChunks . map ((<> "\n") . unparseSSHKey) . map snd . unSSHKeys


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


-- | Determine whether the user identified by their SSH key can access a repo.
accessRepo :: CheckRepoAccessRequest -> SSHHandler RepoAccess
accessRepo CheckRepoAccessRequest{key_id, command} = do
    rows <- query [sql|
                   select id, pk.readonly, pk.verified
                   from "public_key" as pk where id = ?
               |] (Only key_id)
    logDebug (key_id, command, rows)

    let SSHCommandLine command' owner name = command
    case rows of
      []                        -> throwHandlerError NoSSHKey
      _:_:_                     -> throwHandlerError MultipleSSHKeys
      [(_, _, False)]           -> throwHandlerError UnverifiedKey
      [(keyId, readOnly, True)] ->
        case (command', readOnly) of
          (GitReceivePack, True)  -> throwHandlerError $ ReadOnlyKey keyId
          _                       -> routeRepoRequest command' owner name


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
