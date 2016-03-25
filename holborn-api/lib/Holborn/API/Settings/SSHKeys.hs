-- | Fetch some keys:
-- $ curl 127.0.0.1:8002/v1/users/alice/keys

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
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
import Data.Aeson (Value(..), object, (.=))
import Servant

import Holborn.API.Types (AppConf(..), Username, parseSSHKey)
import Holborn.JSON.Settings.SSHKeys (AddKeyData(..), ListKeysRow(..))
import Holborn.Auth (AuthToken(..), Permission(..), hasPermission)
import Holborn.Errors (jsonErrorHandler, APIError(..), JSONCodeableError(..))
import Holborn.API.Auth (getAuthFromToken)
import qualified Holborn.Logging as Log


type API =
         "v1" :> "users" :> Capture "username" Username :> "keys" :> Get '[JSON] [ListKeysRow]
    :<|> "v1" :> "user" :> "keys" :> Capture "id" Int :> Get '[JSON] ListKeysRow
    :<|> "v1" :> Header "Authorization" AuthToken :> "user" :> "keys" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> "v1" :> Header "Authorization" AuthToken :> "user" :> "keys" :> ReqBody '[JSON] AddKeyData :> PostCreated '[JSON] ListKeysRow


-- TODO Tom would really rather have an applicative validator that can
-- check & mark several errors at the same time because that's a much
-- better user experience.
data KeyError = EmptyTitle | InvalidSSHKey


instance JSONCodeableError KeyError where
    toJSON EmptyTitle = (400, object ["title" .= ("Title cannot be empty" :: Text)])
    toJSON InvalidSSHKey = (400, object ["key" .= ("Invalid SSH key" :: Text)])


server :: AppConf -> Server API
server conf = enter jsonErrorHandler $
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
getKey conf keyId = undefined


deleteKey :: AppConf -> Maybe AuthToken -> Int -> ExceptT (APIError KeyError) IO ()
deleteKey appconf@AppConf{conn=conn} token keyId = do
    (userId, permissions) <- getAuthFromToken appconf token
    unless (hasPermission permissions Web) (throwE InsufficientPermissions)

    count <- liftIO $ execute conn [sql|
            delete from "public_key" where id = ? and owner_id = ?
            |] (keyId, userId)
    Log.debug count
    return ()


addKey :: AppConf -> Maybe AuthToken -> AddKeyData -> ExceptT (APIError KeyError) IO ListKeysRow
addKey appconf@AppConf{conn=conn} token AddKeyData{..} = do
    let sshKey = parseSSHKey (encodeUtf8 _AddKeyData_key)
    when (isNothing sshKey) (throwE (SubAPIError InvalidSSHKey))
    when (_AddKeyData_title == "") (throwE (SubAPIError EmptyTitle))
    (userId, permissions) <- getAuthFromToken appconf token
    unless (hasPermission permissions Web) (throwE InsufficientPermissions)

    [Only id_] <- liftIO $ query conn [sql|
            insert into "public_key" (id, name, submitted_pubkey, comparison_pubkey, owner_id, verified, readonly, created)
            values (default, ?, ?, ?, ?, false, true, default) returning id
            |] (_AddKeyData_title, _AddKeyData_key, sshKey, userId)

    [r] <- liftIO $ query conn [sql|
                   select id, submitted_pubkey, name, verified, readonly, created
                   from "public_key" where id = ?
               |] (Only id_ :: Only Integer)
    return r
