-- | Fetch some keys:
-- $ curl 127.0.0.1:8002/v1/users/tom/keys

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Keys
       ( API
       , server
       ) where

import BasicPrelude

import Control.Monad.Trans.Either (EitherT(..), left)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Error (bimapExceptT)
import Servant
import Data.Aeson (Value(..), object, encode)

import Holborn.API.Types (AppConf(..), Username, parseSSHKey, SSHKey)
import Holborn.JSON.Keys (AddKeyData(..), ListKeysRow(..))
import Holborn.Auth (AuthToken(..), userFromToken, Permission(..), hasPermission)
import Holborn.Errors (jsonErrorHandler, GeneralError(..), JSONCodableError(..))

instance FromText AuthToken where
    fromText token = Just (AuthToken (encodeUtf8 token))


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
    toJSON EmptyTitle = (400, object [])
    toJSON InvalidSSHKey = (400, object [])


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
                   from "public_key"
               |] ()
    return r


getKey :: AppConf -> Int -> ExceptT (GeneralError KeyError) IO ListKeysRow
getKey conf keyId = undefined


deleteKey :: AppConf -> Maybe AuthToken -> Int -> ExceptT (GeneralError KeyError) IO ()
deleteKey AppConf{conn=conn} token keyId = do
    (userId, permissions) <- getAuth conn token
    unless (hasPermission permissions Web) (throwE InsufficientPermissions)

    count <- liftIO $ execute conn [sql|
            delete from "public_key" where id = ? and owner_id = ?
            |] (keyId, userId)
    liftIO $ print count
    return ()



addKey :: AppConf -> Maybe AuthToken -> AddKeyData -> ExceptT (GeneralError KeyError) IO ListKeysRow
addKey AppConf{conn=conn} token AddKeyData{..} = do

    when (isNothing (parseSSHKey (encodeUtf8 _AddKeyData_key))) (throwE (SpecificError InvalidSSHKey))
    when (_AddKeyData_title == "") (throwE (SpecificError EmptyTitle))
    (userId, permissions) <- getAuth conn token
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


-- ExceptT trying to auth the user
getAuth conn token = do
    authToken <- case token of
        Nothing -> throwE MissingAuthToken
        Just x -> return x

    maybeUser <- liftIO $ userFromToken conn authToken
    case maybeUser of
        Just (userId, permissions) -> return (userId, permissions)
        Nothing -> throwE InvalidAuthToken
