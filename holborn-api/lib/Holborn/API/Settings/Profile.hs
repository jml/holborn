-- | Profile settings, e.g. full name, location, ...

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Settings.Profile
       ( API
       , server
       ) where

import BasicPrelude

import Control.Monad.Trans.Either (left)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (Value(..), object, (.=))
import Servant

import Holborn.API.Types (AppConf(..), Username)
import Holborn.JSON.Settings.Profile (ProfileData(..))
import Holborn.Auth (AuthToken(..), Permission(..), hasPermission, getAuthFromToken)
import Holborn.Errors (jsonErrorHandler, GeneralError(..), JSONCodableError(..))


type API =
         "v1" :> "users" :> Capture "username" Username :> Get '[JSON] ProfileData
    :<|> "v1" :> Header "Authorization" AuthToken :> "user" :> Get '[JSON] ProfileData
    :<|> "v1" :> Header "Authorization" AuthToken :> "user" :> ReqBody '[JSON] ProfileData :> Post '[JSON] ()


data Error = InvalidUrl


instance JSONCodableError Error where
    toJSON InvalidUrl = (400, object ["url" .= ("Not a valid URL" :: Text)])


server :: AppConf -> Server API
server conf = enter jsonErrorHandler $
    getUser conf
    :<|> getAuthorizedUser conf
    :<|> postAuthorizedUser conf


getUser :: AppConf -> Username -> ExceptT (GeneralError Error) IO ProfileData
getUser conf username = undefined


getAuthorizedUser :: AppConf -> Maybe AuthToken -> ExceptT (GeneralError Error) IO ProfileData
getAuthorizedUser conf token = undefined


postAuthorizedUser :: AppConf -> Maybe AuthToken -> ProfileData -> ExceptT (GeneralError Error) IO ()
postAuthorizedUser conf token newProfile = undefined
