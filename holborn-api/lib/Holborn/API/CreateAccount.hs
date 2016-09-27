{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeOperators      #-}

module Holborn.API.CreateAccount
       ( API
       , server
       ) where

import HolbornPrelude

import Data.Aeson (FromJSON, object, (.=))
import Servant

import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , toServantHandler
  , query
  )
import Holborn.API.Types (DexMail)
import Holborn.CommonTypes.Repo (OwnerName)


type API =
    "create-account"
    :> Header "x-dex-email" DexMail
    :> ReqBody '[JSON] CreateAccountRequest
    :> Post '[JSON] ()


data CreateAccountRequest = CreateAccountRequest
    { username :: OwnerName
    } deriving (Show, Generic)

instance FromJSON CreateAccountRequest

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
        insert into "auth_user" (password, is_superuser, username, first_name, last_name, email, is_staff, is_active, date_joined)
        values ('', false, ?, '', '', ?, false, true, now() at time zone 'utc') returning id
        |] (username, dexMail)
    return ()
