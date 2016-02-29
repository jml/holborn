-- | Fetch some keys:
-- $ curl 127.0.0.1:8002/v1/users/alice/keys

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Keys
       ( API
       , server
       ) where

import BasicPrelude

import Control.Monad.Trans.Either (left)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Servant
import Data.Aeson (Value(..), object, (.=))

import Holborn.API.Types (AppConf(..), Username, parseSSHKey)
import Holborn.JSON.Keys (AddKeyData(..), ListKeysRow(..))
import Holborn.Auth (AuthToken(..), Permission(..), hasPermission, getAuthFromToken)
import Holborn.Errors (jsonErrorHandler, GeneralError(..), JSONCodableError(..))


type API =
         "v1" :> "users" :> Capture "username" Username :> "keys" :> Get '[JSON] [ListKeysRow]
    :<|> "v1" :> "user" :> "keys" :> Capture "id" Int :> Get '[JSON] ListKeysRow
    :<|> "v1" :> Header "Authorization" AuthToken :> "user" :> "keys" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> "v1" :> Header "Authorization" AuthToken :> "user" :> "keys" :> ReqBody '[JSON] AddKeyData :> Post '[JSON] ListKeysRow


-- TODO Tom would really rather have an applicative validator that can
-- check & mark several errors at the same time because that's a much
-- better user experience.
data KeyError = EmptyTitle | InvalidSSHKey


instance JSONCodableError KeyError where
    toJSON EmptyTitle = (400, object ["title" .= ("Title cannot be empty" :: Text)])
    toJSON InvalidSSHKey = (400, object ["key" .= ("Invalid SSH key" :: Text)])


server :: AppConf -> Server API
server conf = enter jsonErrorHandler $
    (listKeys conf)
    :<|> (getKey conf)
    :<|> (deleteKey conf)
    :<|> (addKey conf)


listKeys :: AppConf -> Username -> ExceptT (GeneralError KeyError) IO [ListKeysRow]
listKeys AppConf{conn=conn} username = do
    r <- liftIO $ query conn [sql|
                   select id, pubkey, name, verified, readonly, created
                   from "public_key" where owner_id = (select id from "user" where username = ?)
               |] (Only username)
    return r


getKey :: AppConf -> Int -> ExceptT (GeneralError KeyError) IO ListKeysRow
getKey conf keyId = undefined


deleteKey :: AppConf -> Maybe AuthToken -> Int -> ExceptT (GeneralError KeyError) IO ()
deleteKey appconf@AppConf{conn=conn} token keyId = do
    (userId, permissions) <- getAuthFromToken appconf token
    unless (hasPermission permissions Web) (throwE InsufficientPermissions)

    count <- liftIO $ execute conn [sql|
            delete from "public_key" where id = ? and owner_id = ?
            |] (keyId, userId)
    liftIO $ print count
    return ()


addKey :: AppConf -> Maybe AuthToken -> AddKeyData -> ExceptT (GeneralError KeyError) IO ListKeysRow
addKey appconf@AppConf{conn=conn} token AddKeyData{..} = do

    when (isNothing (parseSSHKey (encodeUtf8 _AddKeyData_key))) (throwE (SpecificError InvalidSSHKey))
    when (_AddKeyData_title == "") (throwE (SpecificError EmptyTitle))
    (userId, permissions) <- getAuthFromToken appconf token
    unless (hasPermission permissions Web) (throwE InsufficientPermissions)

    [Only id_] <- liftIO $ query conn [sql|
            insert into "public_key" (id, name, pubkey, owner_id, verified, readonly, created)
            values (default, ?, ?, ?, false, true, default) returning id
            |] (_AddKeyData_title, _AddKeyData_key, userId)

    [r] <- liftIO $ query conn [sql|
                   select id, pubkey, name, verified, readonly, created
                   from "public_key" where id = ?
               |] (Only id_ :: Only Integer)
    return r
