{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes                #-}

module Holborn.API.Keys
       ( API
       , server
       ) where

import BasicPrelude

import Holborn.API.Types (AppConf(..), Username)
import Control.Monad.Trans.Either (EitherT, left)
import Holborn.JSON.Keys (AddKeyData(..), ListKeysRow(..))
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Servant

type API =
         "v1" :> "users" :> Capture "username" Username :> "keys" :> Get '[JSON] [ListKeysRow]
    :<|> "v1" :> "user" :> "keys" :> Capture "id" Int :> Get '[JSON] ListKeysRow
    :<|> "v1" :> "user" :> "keys" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> "v1" :> "user" :> "keys" :> ReqBody '[JSON] AddKeyData :> Post '[JSON] ListKeysRow


server :: AppConf -> Server API
server conf =
    (listKeys conf)
    :<|> (getKey conf)
    :<|> (deleteKey conf)
    :<|> (addKey conf)


listKeys :: AppConf -> Username -> EitherT ServantErr IO [ListKeysRow]
listKeys conf username = undefined

getKey :: AppConf -> Int -> EitherT ServantErr IO ListKeysRow
getKey conf keyId = undefined

deleteKey :: AppConf -> Int -> EitherT ServantErr IO ()
deleteKey conf keyId = undefined

addKey :: AppConf -> AddKeyData -> EitherT ServantErr IO ListKeysRow
addKey AppConf{conn=conn} AddKeyData{..} = do
    [Only id_] <- liftIO $ query conn [sql|
            insert into "public_key" (id, name, pubkey, owner_id, verified, readonly, created)
            values (default, ?, ?, ?, false, true, default) returning id
            |] (_AddKeyData_title, _AddKeyData_key, 1::Integer)

    [r] <- liftIO $ query conn [sql|
                   select id, pubkey, name, verified, readonly, created
                   from "public_key" where id = ?
               |] (Only id_ :: Only Integer)
    return r
