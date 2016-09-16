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

import Holborn.API.Config (Config)
import Holborn.JSON.Settings.Profile (ProfileData(..))
import Holborn.API.Types (Username, DexMail)
import Holborn.API.Auth (getUserId)
import Holborn.API.Internal (APIHandler, JSONCodeableError(..), toServantHandler, throwHandlerError, query)


type API =
         "users" :> Capture "username" Username :> Get '[JSON] ProfileData
    :<|> Header "x-dex-email" DexMail :> "user" :> Get '[JSON] ProfileData
    :<|> Header "x-dex-email" DexMail :> "user" :> ReqBody '[JSON] ProfileData :> Post '[JSON] ()


data Error = InvalidUrl | UserNotFound Text | UserNotInDb


instance JSONCodeableError Error where
    toJSON InvalidUrl = (400, object ["url" .= ("Not a valid URL" :: Text)])
    toJSON (UserNotFound x) = (404, object ["message" .= ("User " <> x <> " not found")])
    toJSON UserNotInDb = (400, object ["message" .= ("unsure about the problem!" :: Text)])


server :: Config -> Server API
server conf = enter (toServantHandler conf) $
    getUser
    :<|> getAuthorizedUser
    :<|> postAuthorizedUser


getUser :: Username -> APIHandler Error ProfileData
getUser username = do
    r <- query [sql|
                   select id, username, date_joined
                   from auth_user where username = ?
               |] (Only username)

    case r of
        [] -> throwHandlerError (UserNotFound (show username))
        [(id_, un, date_joined)] -> return (ProfileData id_ un "about this user TODO fetch from DB" date_joined)
        _ -> terror $ "Multiple users found in the database for " ++ show username ++ ". Found: " ++ show r


-- TODO The function to fetch the current user should go somewhere
-- other than profile settings?
getAuthorizedUser :: Maybe DexMail -> APIHandler Error ProfileData
getAuthorizedUser dexMail = do
    userId <- getUserId dexMail
    r <- query [sql|
                   select username, date_joined
                   from auth_user where id = ?
               |] (Only userId)
    case r of
        [(uname, date_joined)] -> pure (ProfileData userId uname "about this user TODO fetch from DB" date_joined)
        _ -> throwHandlerError UserNotInDb


postAuthorizedUser :: Maybe DexMail -> ProfileData -> APIHandler Error ()
postAuthorizedUser _dexMail _newProfile = undefined
