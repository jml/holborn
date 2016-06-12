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

import Holborn.API.Config (AppConf(..))
import Holborn.JSON.SSHRepoCommunication (parseSSHKey)
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
data KeyError = EmptyTitle | InvalidSSHKey


instance JSONCodeableError KeyError where
    toJSON EmptyTitle = (400, object ["title" .= ("Title cannot be empty" :: Text)])
    toJSON InvalidSSHKey = (400, object ["key" .= ("Invalid SSH key" :: Text)])


server :: AppConf -> Server API
server conf = enter (toServantHandler conf) $
    listKeys
    :<|> getKey
    :<|> deleteKey
    :<|> addKey


listKeys :: Username -> APIHandler KeyError [ListKeysRow]
listKeys username =
    query [sql|
              select id, comparison_pubkey, name, verified, readonly, created
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
addKey username AddKeyData{..} = do
    let sshKey = parseSSHKey (encodeUtf8 _AddKeyData_key)
    when (isNothing sshKey) (throwHandlerError InvalidSSHKey)
    when (_AddKeyData_title == "") (throwHandlerError EmptyTitle)
    userId <- getUserId username

    [Only id_] <- query [sql|
            insert into "public_key" (id, name, submitted_pubkey, comparison_pubkey, owner_id, verified, readonly, created)
            values (default, ?, ?, ?, ?, false, true, default) returning id
            |] (_AddKeyData_title, _AddKeyData_key, sshKey, userId)

    [r] <- query [sql|
                   select id, submitted_pubkey, name, verified, readonly, created
                   from "public_key" where id = ?
               |] (Only id_ :: Only Integer)
    return r
