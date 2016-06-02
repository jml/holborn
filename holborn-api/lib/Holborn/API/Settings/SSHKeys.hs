-- | Fetch some keys:
-- $ curl 127.0.0.1:8002/v1/users/alice/keys

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Settings.SSHKeys
       ( API
       , server
       ) where

import BasicPrelude

import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (object, (.=))
import Servant

import Holborn.API.Config (AppConf(..))
import Holborn.JSON.SSHRepoCommunication (parseSSHKey)
import Holborn.JSON.Settings.SSHKeys (AddKeyData(..), ListKeysRow(..))
import Holborn.Errors (APIError(..))
import Holborn.API.Internal (JSONCodeableError(..), toServantHandler)
import Holborn.API.Auth (getUserId)
import qualified Holborn.Logging as Log
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
server conf = enter toServantHandler $
    listKeys conf
    :<|> getKey conf
    :<|> deleteKey conf
    :<|> addKey conf


listKeys :: AppConf -> Username -> ExceptT (APIError KeyError) IO [ListKeysRow]
listKeys AppConf{conn=conn} username = do
    r <- liftIO $ query conn [sql|
                   select id, comparison_pubkey, name, verified, readonly, created
                   from "public_key" where owner_id = (select id from "user" where username = ?)
               |] (Only username)
    return r


getKey :: AppConf -> Int -> ExceptT (APIError KeyError) IO ListKeysRow
getKey _conf _keyId = undefined


deleteKey :: AppConf -> Maybe Username -> Int -> ExceptT (APIError KeyError) IO ()
deleteKey appconf@AppConf{conn=conn} username keyId = do
    userId <- getUserId appconf username

    count <- liftIO $ execute conn [sql|
            delete from "public_key" where id = ? and owner_id = ?
            |] (keyId, userId)
    Log.debug count
    return ()


addKey :: AppConf -> Maybe Username -> AddKeyData -> ExceptT (APIError KeyError) IO ListKeysRow
addKey appconf@AppConf{conn=conn} username AddKeyData{..} = do
    let sshKey = parseSSHKey (encodeUtf8 _AddKeyData_key)
    when (isNothing sshKey) (throwE (SubAPIError InvalidSSHKey))
    when (_AddKeyData_title == "") (throwE (SubAPIError EmptyTitle))
    userId <- getUserId appconf username

    [Only id_] <- liftIO $ query conn [sql|
            insert into "public_key" (id, name, submitted_pubkey, comparison_pubkey, owner_id, verified, readonly, created)
            values (default, ?, ?, ?, ?, false, true, default) returning id
            |] (_AddKeyData_title, _AddKeyData_key, sshKey, userId)

    [r] <- liftIO $ query conn [sql|
                   select id, submitted_pubkey, name, verified, readonly, created
                   from "public_key" where id = ?
               |] (Only id_ :: Only Integer)
    return r
