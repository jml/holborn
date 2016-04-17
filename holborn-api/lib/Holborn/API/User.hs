{-# LANGUAGE QuasiQuotes                #-}
module Holborn.API.User
       ( signup
       , listUsers
       , getUser
       ) where

import BasicPrelude
import Control.Error (ExceptT, throwE)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Types (ApiError (..), Email, Password, Username)

import qualified Holborn.JSON.User as U
import qualified Holborn.JSON.Response as U


signup :: Connection -> Username -> Email -> Password -> ExceptT ApiError IO ()
signup conn u e p = do
    void $ catchJust constraintViolation
        (liftIO (execute conn [sql|
            insert into "user" (id, username, signup_email, password, created)
            values (default, ?, ?, ?, default)
            |] (u, e, p)))
        handleError
  where
    handleError (UniqueViolation "user_username_key") = throwE (UserAlreadyExists u)
    handleError unknown = throwE (UnexpectedConstraintViolation (show unknown))


listUsers :: Connection -> Int -> ExceptT ApiError IO (U.PaginatedResponse [U.ListUsersRow])
listUsers conn startId = do
    r <- liftIO $ (query conn [sql|
        select id, username
        from "user" where id > ? order by id limit 50
        |] (Only startId) :: IO [U.ListUsersRow])
    let next = case r of
                   [] -> ""
                   _ -> show (maximum (map U._ListUsersRow_id r))
    return (U.PaginatedResponse r next)


getUser :: Connection -> Username -> ExceptT ApiError IO U.ListUsersRow
getUser conn username = do
    r <- liftIO $ (query conn [sql|
        select id, username
        from "user" where username = ?
        |] (Only username) :: IO [U.ListUsersRow])
    case r of
        [] -> throwE (UserNotFound username)
        [row] -> return row
        _ -> terror "getUser returned more than 1 result despite unique constraint"
