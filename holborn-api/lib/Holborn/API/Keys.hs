{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}

module Holborn.API.Keys
       ( API
       , server
       ) where

import BasicPrelude

import Holborn.API.Types (AppConf(..), Username)
import Control.Monad.Trans.Either (EitherT, left)
import Holborn.JSON.Keys (AddKeyData(..), ListKeysRow(..))

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
addKey conf json = undefined
