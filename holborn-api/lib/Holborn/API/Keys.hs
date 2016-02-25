-- | Add a key e.g.
-- Post a new key (we don't do auth yet).
-- $ curl 127.0.0.1:8002/v1/user/keys/ -H "content-type: application/json" -d '{"key": "key", "title": "tom"}'
-- Fetch some keys:
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

import Holborn.API.Types (AppConf(..), Username, parseSSHKey, SSHKey)
import Holborn.JSON.Keys (AddKeyData(..), ListKeysRow(..))
import Holborn.Auth (AuthToken(..), userFromToken, Permission(..), hasPermission)


instance FromText AuthToken where
    fromText token = Just (AuthToken (encodeUtf8 token))


type API =
         "v1" :> "users" :> Capture "username" Username :> "keys" :> Get '[JSON] [ListKeysRow]
    :<|> "v1" :> "user" :> "keys" :> Capture "id" Int :> Get '[JSON] ListKeysRow
    :<|> "v1" :> "user" :> "keys" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> "v1" :> Header "Authorization" AuthToken :> "user" :> "keys" :> ReqBody '[JSON] AddKeyData :> Post '[JSON] ListKeysRow


-- TODO Tom would really rather have an applicative validator that can
-- check & mark several errors at the same time because that's a much
-- better user experience.
data KeyError =
    EmptyTitle
    | InvalidSSHKey
    | MissingAuthToken
    | InvalidAuthToken
    | InsufficientPermissions

keyErrors :: ExceptT KeyError IO :~> EitherT ServantErr IO
keyErrors = Nat (exceptTToEitherT . bimapExceptT handleError id)
  where
    exceptTToEitherT = EitherT . runExceptT
    handleError EmptyTitle = err400 { errBody = "{\"title\": \"Title can not be empty\"}" }
    handleError InvalidSSHKey = err400 { errBody = "{\"key\": \"SSH key invalid\"}" }
    handleError MissingAuthToken = err401 { errBody = "{\"error\": \"Authorization token missing\"}" }
    handleError InvalidAuthToken = err401 { errBody = "{\"error\": \"Authorization token invalid\"}" }
    handleError InsufficientPermissions = err403 { errBody = "{\"error\": \"Not permitted\"}" }


server :: AppConf -> Server API
server conf = enter keyErrors $
    (listKeys conf)
    :<|> (getKey conf)
    :<|> (deleteKey conf)
    :<|> (addKey conf)


listKeys :: AppConf -> Username -> ExceptT KeyError IO [ListKeysRow]
listKeys AppConf{conn=conn} username = do
    r <- liftIO $ query conn [sql|
                   select id, pubkey, name, verified, readonly, created
                   from "public_key"
               |] ()
    return r


getKey :: AppConf -> Int -> ExceptT KeyError IO ListKeysRow
getKey conf keyId = undefined


deleteKey :: AppConf -> Int -> ExceptT KeyError IO ()
deleteKey conf keyId = undefined



addKey :: AppConf -> Maybe AuthToken -> AddKeyData -> ExceptT KeyError IO ListKeysRow
addKey AppConf{conn=conn} token AddKeyData{..} = do

    when (isNothing (parseSSHKey (encodeUtf8 _AddKeyData_key))) (throwE InvalidSSHKey)
    when (_AddKeyData_title == "") (throwE EmptyTitle)
    (userId, permissions) <- getAuth
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

  where
    getAuth = do
      authToken <- case token of
          Nothing -> throwE MissingAuthToken
          Just x -> return x

      maybeUser <- liftIO $ userFromToken conn authToken
      case maybeUser of
          Just (userId, permissions) -> return (userId, permissions)
          Nothing -> throwE InvalidAuthToken
