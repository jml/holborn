{-# LANGUAGE QuasiQuotes #-}

module Holborn.API.Auth
       ( getUserId
       ) where

import BasicPrelude

import Control.Monad.Trans.Except (throwE)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Connection, Only (..), query)

import Holborn.API.Config (AppConf(..))
import Holborn.API.Internal (APIError(..), APIHandler)
import Holborn.API.Types (Username)


type UserId = Int

userFromUsername :: Connection -> Username -> IO (Maybe UserId)
userFromUsername c username = do
    r <- query c [sql|
        insert into "user"  (username, email) select ?, 'email@test' where not exists (select 1 from "user" where username = ?);
        select id from "user" where username = ?
    |] (username, username, username) :: IO [Only UserId]

    return $ case r of
      [Only one] -> Just one
      _ -> Nothing


-- ExceptT trying to auth the user
getUserId :: AppConf -> Maybe Username -> APIHandler a UserId
getUserId _ Nothing = throwE MissingAuthToken
getUserId AppConf{conn} (Just username) = do
    maybeUser <- liftIO (userFromUsername conn username)
    case maybeUser of
        Just userId -> pure userId
        Nothing -> throwE InvalidAuthToken
