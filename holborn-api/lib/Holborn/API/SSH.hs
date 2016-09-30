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
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.Internal (RowParser)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Web.HttpApiData (toUrlPiece)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , RepoAccess(..)
  , query
  , queryWith
  , routeRepoRequest
  , throwHandlerError
  , toServantHandler
  )
import Holborn.CommonTypes.Repo (OwnerName, RepoName)
import Holborn.JSON.SSHRepoCommunication
  ( KeyType(..)
  , GitCommand(..)
  , SSHCommandLine(..)
  , SSHKey
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
  | NoSuchRepo OwnerName RepoName
  deriving (Eq, Show)

instance JSONCodeableError SSHError where
  toJSON NoSSHKey = (401, object [ "message" .= ("Could not find SSH key" :: Text) ])
  toJSON MultipleSSHKeys = (500, object [ "message" .= ("Found multiple matching SSH keys" :: Text) ])
  toJSON (ReadOnlyKey keyId) = (403, object [ "message" .= ("Cannot push to repository with read-only key" :: Text)
                                            , "keyId" .= keyId
                                            ])
  toJSON UnverifiedKey = (403, object [ "message" .= ("Cannot access repositories with key that has not been verified" :: Text)])
  toJSON (NoSuchRepo owner repo) = (404, object [ "message" .= ("No such repository: " <> toUrlPiece owner <> "/" <> toUrlPiece repo) ])


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
  -- PUPPY: We are including unverfied keys in this "authorized keys" list,
  -- which is a potential security escalation vector.
  rows <- queryWith
    parser
    [sql|select id, "type", "key", comment
         from core_sshkey
         where "key" = ?
         |] (Only key)
  return $ SSHKeys rows

  where
    parser :: RowParser (KeyId, SSHKey)
    parser = do
      keyId <- field
      sshKey <- fromRow
      pure (keyId, sshKey)


-- | Determine whether the user identified by their SSH key can access a repo.
accessRepo :: CheckRepoAccessRequest -> SSHHandler RepoAccess
accessRepo CheckRepoAccessRequest{keyId, command} = do
    rows <- query [sql|
                   select id, readonly, verified
                   from core_sshkey where id = ?
               |] (Only keyId)

    let SSHCommandLine command' owner name = command
    case rows of
      []                        -> throwHandlerError NoSSHKey
      _:_:_                     -> throwHandlerError MultipleSSHKeys
      [(_, _, False)]           -> throwHandlerError UnverifiedKey
      [(keyId', readOnly, True)] ->
        case (command', readOnly) of
          (GitReceivePack, True)  -> throwHandlerError $ ReadOnlyKey keyId'
          _                       -> do
            access <- routeRepoRequest command' owner name
            maybe (throwHandlerError $ NoSuchRepo owner name) pure access


data CheckKeyRequest = CheckKeyRequest
    { key :: Text
    , keyType :: KeyType
    } deriving (Show, Generic, Eq, Ord)
instance FromJSON CheckKeyRequest
instance ToJSON CheckKeyRequest

type KeyId = Int


data CheckRepoAccessRequest = CheckRepoAccessRequest
    { keyId :: KeyId
    , command :: SSHCommandLine
    } deriving (Show, Generic)

instance FromJSON CheckRepoAccessRequest
instance ToJSON CheckRepoAccessRequest
