-- | Profile settings, e.g. full name, location, ...

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Settings.Profile
       ( API
       , server
       ) where

import HolbornPrelude

import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Aeson (object, (.=))
import Servant

import Holborn.API.Config (AppConf(..))
import Holborn.JSON.Settings.Profile (ProfileData(..))
import Holborn.API.Types (Username)
import Holborn.API.Auth (getUserId)
import Holborn.API.Internal (APIHandler, JSONCodeableError(..), toServantHandler, throwHandlerError, query)


type API =
         "users" :> Capture "username" Username :> Get '[JSON] ProfileData
    :<|> Header "GAP-Auth" Username :> "user" :> Get '[JSON] ProfileData
    :<|> Header "GAP-Auth" Username :> "user" :> ReqBody '[JSON] ProfileData :> Post '[JSON] ()


data Error = InvalidUrl | UserNotFound Text | UserNotInDb


instance JSONCodeableError Error where
    toJSON InvalidUrl = (400, object ["url" .= ("Not a valid URL" :: Text)])
    toJSON (UserNotFound x) = (404, object ["message" .= ("User " <> x <> " not found")])
    toJSON UserNotInDb = (400, object ["message" .= ("unsure about the problem!" :: Text)])


server :: AppConf -> Server API
server conf = enter (toServantHandler conf) $
    getUser
    :<|> getAuthorizedUser
    :<|> postAuthorizedUser


getUser :: Username -> APIHandler Error ProfileData
getUser username = do
    r <- query [sql|
                   select id, username, created
                   from "user" where username = ?
               |] (Only username)

    case r of
        [] -> throwHandlerError (UserNotFound (show username))
        [(id_, un, created)] -> return (ProfileData id_ un "about this user TODO fetch from DB" created)
        _ -> terror $ "Multiple users found in the database for " ++ show username ++ ". Found: " ++ show r


-- TODO The function to fetch the current user should go somewhere
-- other than profile settings?
getAuthorizedUser :: Maybe Username -> APIHandler Error ProfileData
getAuthorizedUser username = do
    userId <- getUserId username
    r <- query [sql|
                   select username, created
                   from "user" where id = ?
               |] (Only userId)
    case r of
        [(uname, created)] -> pure (ProfileData userId uname "about this user TODO fetch from DB" created)
        _ -> throwHandlerError UserNotInDb


postAuthorizedUser :: Maybe Username -> ProfileData -> APIHandler Error ()
postAuthorizedUser _username _newProfile = undefined
