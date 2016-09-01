{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}


module Holborn.API.CreateAccount
       ( API
       , server
       ) where

import HolbornPrelude

import Data.Aeson (object, (.=))
import Servant

import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , toServantHandler
  , query
  )
import Holborn.JSON.CreateAccount (CreateAccountRequest(..))
import Holborn.API.Types (DexMail)


type API =
    "create-account"
    :> Header "x-dex-email" DexMail
    :> ReqBody '[JSON] CreateAccountRequest
    :> Post '[JSON] ()


data CreateAccountError = AlreadyExists | PermissionDenied

instance JSONCodeableError CreateAccountError where
    toJSON AlreadyExists = (400, object ["message" .= ("Account with this username already exists" :: Text)])
    toJSON PermissionDenied = (400, object ["message" .= ("You are not allowed to create this account" :: Text)])


server :: Config -> Server API
server conf =
  enter (toServantHandler conf) createAccount


createAccount :: Maybe DexMail -> CreateAccountRequest -> APIHandler CreateAccountError ()
createAccount dexMail CreateAccountRequest{..} = do
    [Only (_ :: Int)] <- query [sql|
        insert into "user" (username, email) values (?, ?) returning id
        |] (username, dexMail)
    return ()
