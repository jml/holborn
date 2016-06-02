-- | Profile settings, e.g. full name, location, ...

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.Settings.Profile
       ( API
       , server
       ) where

import BasicPrelude

import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Aeson (object, (.=))
import Servant

import Holborn.API.Config (AppConf(..))
import Holborn.JSON.Settings.Profile (ProfileData(..))
import Holborn.API.Types (Username)
import Holborn.API.Auth (getUserId)
import Holborn.API.Internal (APIHandler, JSONCodeableError(..), toServantHandler, handlerError)


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
server conf = enter toServantHandler $
    getUser conf
    :<|> getAuthorizedUser conf
    :<|> postAuthorizedUser conf


getUser :: AppConf -> Username -> APIHandler Error ProfileData
getUser AppConf{conn} username = do
    r <- liftIO $ query conn [sql|
                   select id, username, created
                   from "user" where username = ?
               |] (Only username)

    case r of
        [] -> handlerError (UserNotFound (show username))
        [(id_, un, created)] -> return (ProfileData id_ un "about this user TODO fetch from DB" created)
        _ -> terror $ "Multiple users found in the database for " ++ show username ++ ". Found: " ++ show r


-- TODO The function to fetch the current user should go somewhere
-- other than profile settings?
getAuthorizedUser :: AppConf -> Maybe Username -> APIHandler Error ProfileData
getAuthorizedUser conf@AppConf{conn} username = do
    userId <- getUserId conf username
    r <- liftIO $ query conn [sql|
                   select username, created
                   from "user" where id = ?
               |] (Only userId)
    case r of
        [(uname, created)] -> pure (ProfileData userId uname "about this user TODO fetch from DB" created)
        _ -> handlerError UserNotInDb -- TODO more informative error by encrypting context and sending it to the user


postAuthorizedUser :: AppConf -> Maybe Username -> ProfileData -> APIHandler Error ()
postAuthorizedUser _conf _username _newProfile = undefined
