-- | Fetch some keys:
--   curl 127.0.0.1:8002/v1/users/alice/keys

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Settings.SSHKeys
       ( API
       , server
       ) where

import HolbornPrelude

import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.Internal (RowParser)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant

import Holborn.API.Config (Config)
import Holborn.JSON.SSHRepoCommunication (SSHKey(SSHKey), parseSSHKey)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , IntegrityError(..)
  , corruptDatabase
  , toServantHandler
  , throwHandlerError
  , execute
  , executeWith
  , queryWith
  , logDebug
  )
import Holborn.API.Auth (getUserId)
import Holborn.API.Types (Username, DexMail)


type API =
         "users" :> Capture "username" Username :> "keys" :> Get '[JSON] [ListKeysRow] -- list other user keys
    :<|> Header "x-dex-email" DexMail :> "user" :> "keys" :> Get '[JSON] [ListKeysRow] -- list own keys
    :<|> Header "x-dex-email" DexMail :> "user" :> "keys" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> Header "x-dex-email" DexMail :> "user" :> "keys" :> ReqBody '[JSON] AddKeyData :> PostCreated '[JSON] ListKeysRow

data ListKeysRow = ListKeysRow
    { id :: Int
    , key :: SSHKey
    , verified :: Bool
    , readonly :: Bool
    , created_at :: UTCTime
    } deriving (Show, Generic)

instance ToJSON ListKeysRow

data AddKeyData = AddKeyData
    { key :: Text
    } deriving (Show, Generic)

instance ToJSON AddKeyData
instance FromJSON AddKeyData

-- TODO Tom would really rather have an applicative validator that can
-- check & mark several errors at the same time because that's a much
-- better user experience.
data KeyError = InvalidSSHKey | KeyAlreadyExists

instance JSONCodeableError KeyError where
    toJSON InvalidSSHKey = (400, object ["key" .= ("Invalid SSH key" :: Text)])
    toJSON KeyAlreadyExists = (400, object ["message" .= ("Key already exists" :: Text)])


server :: Config -> Server API
server conf = enter (toServantHandler conf) $
    listKeys
    :<|> listMyKeys
    :<|> deleteKey
    :<|> addKey


listKeys :: Username -> APIHandler KeyError [ListKeysRow]
listKeys username =
    queryWith parseListKeys [sql|
              select id, "type", "key", comment, verified, readonly, created_at
              from core_sshkey where owner_id = (select id from auth_user where username = ?)
          |] (Only username)

listMyKeys :: Maybe DexMail -> APIHandler KeyError [ListKeysRow]
listMyKeys dexMail = do
    userId <- getUserId dexMail
    queryWith parseListKeys [sql|
              select id, "type", "key", comment, verified, readonly, created_at
              from core_sshkey where owner_id = ?
          |] (Only userId)


deleteKey :: Maybe DexMail -> Int -> APIHandler KeyError ()
deleteKey dexMail keyId = do
    userId <- getUserId dexMail

    count <- execute [sql|
            delete from core_sshkey where id = ? and owner_id = ?
            |] (keyId, userId)
    logDebug count
    return ()


addKey :: Maybe DexMail -> AddKeyData -> APIHandler KeyError ListKeysRow
addKey dexMail (AddKeyData key) = do
    userId <- getUserId dexMail
    sshKey <- maybe (throwHandlerError InvalidSSHKey) pure $ parseSSHKey (encodeUtf8 key)
    let SSHKey keyType keyData comment = sshKey

    result <- executeWith parseListKeys [sql|
            insert into core_sshkey
              ( id
              , submitted_key
              , "type"
              , "key"
              , comment
              , owner_id
              , verified
              , readonly
              , created_at
              )
            values (default, ?, ?, ?, ?, ?, false, true, default)
              returning id, "type", "key", comment, verified, readonly, created_at
            |] (key, keyType, keyData, comment, userId)

    case result of
      Left DuplicateValue -> throwHandlerError KeyAlreadyExists
      Left e -> terror $ "Unexpected data error in addKey: " <> show e
      Right [r] -> pure r
      Right _ -> corruptDatabase "Inserting key in addKey returned something weird"


parseListKeys :: RowParser ListKeysRow
parseListKeys = do
  keyId <- field
  sshKey <- fromRow
  verified <- field
  readonly <- field
  created <- field
  pure $ ListKeysRow keyId sshKey verified readonly created
