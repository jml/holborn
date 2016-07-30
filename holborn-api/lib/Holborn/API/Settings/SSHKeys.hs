-- | Fetch some keys:
--   curl 127.0.0.1:8002/v1/users/alice/keys

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Settings.SSHKeys
       ( API
       , server
       ) where

import HolbornPrelude

import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Aeson (object, (.=))
import Servant

import Holborn.API.Config (Config)
import Holborn.JSON.SSHRepoCommunication (SSHKey(SSHKey), parseSSHKey)
import Holborn.JSON.Settings.SSHKeys (AddKeyData(..), ListKeysRow(..))
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , toServantHandler
  , throwHandlerError
  , execute
  , query
  , logDebug
  )
import Holborn.API.Auth (getUserId)
import Holborn.API.Types (Username)


type API =
         "users" :> Capture "username" Username :> "keys" :> Get '[JSON] [ListKeysRow]
    :<|> "user" :> "keys" :> Capture "id" Int :> Get '[JSON] ListKeysRow
    :<|> Header "GAP-Auth" Username :> "user" :> "keys" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> Header "GAP-Auth" Username :> "user" :> "keys" :> ReqBody '[JSON] AddKeyData :> PostCreated '[JSON] ListKeysRow


-- TODO Tom would really rather have an applicative validator that can
-- check & mark several errors at the same time because that's a much
-- better user experience.
data KeyError = InvalidSSHKey


instance JSONCodeableError KeyError where
    toJSON InvalidSSHKey = (400, object ["key" .= ("Invalid SSH key" :: Text)])


server :: Config -> Server API
server conf = enter (toServantHandler conf) $
    listKeys
    :<|> getKey
    :<|> deleteKey
    :<|> addKey


listKeys :: Username -> APIHandler KeyError [ListKeysRow]
listKeys username =
    query [sql|
              select id, submitted_pubkey, verified, readonly, created
              from "public_key" where owner_id = (select id from "user" where username = ?)
          |] (Only username)


getKey :: Int -> APIHandler KeyError ListKeysRow
getKey _keyId = undefined


deleteKey :: Maybe Username -> Int -> APIHandler KeyError ()
deleteKey username keyId = do
    userId <- getUserId username

    count <- execute [sql|
            delete from "public_key" where id = ? and owner_id = ?
            |] (keyId, userId)
    logDebug count
    return ()


addKey :: Maybe Username -> AddKeyData -> APIHandler KeyError ListKeysRow
addKey username (AddKeyData key) = do
    userId <- getUserId username
    let sshKey = parseSSHKey (encodeUtf8 key)
    when (isNothing sshKey) (throwHandlerError InvalidSSHKey)
    let (Just (SSHKey keyType keyData comment fingerprint)) = sshKey

    -- TODO: Use 'returning' to avoid two roundtrips.
    [Only id_] <- query [sql|
            insert into "public_key"
              ( id
              , submitted_pubkey
              , "type"
              , "key"
              , comment
              , fingerprint
              , owner_id
              , verified
              , readonly
              , created
              )
            values (default, ?, ?, ?, ?, ?, ?, false, true, default) returning id
            |] (key, keyType, keyData, comment, fingerprint, userId)

    [r] <- query [sql|
                   select id, submitted_pubkey, verified, readonly, created
                   from "public_key" where id = ?
               |] (Only id_ :: Only Integer)
    return r
