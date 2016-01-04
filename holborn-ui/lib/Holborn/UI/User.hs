{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
module Holborn.UI.User
       ( signup
       , listUsers
       ) where

import BasicPrelude
import Control.Error (ExceptT, throwE)
import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.UI.Types (ApiError (..), Email, Password, Username)

import qualified Holborn.Models.Users as U


signup :: Connection -> Username -> Email -> Password -> ExceptT ApiError IO ()
signup conn u e p = do
    res <- catchJust constraintViolation
        (liftIO (execute conn [sql|
            insert into "user" (id, username, signup_email, password, created)
            values (default, ?, ?, ?, default)
            |] (u, e, p)))
        handle
    return ()
  where
    handle (UniqueViolation "username") = throwE (UserAlreadyExists u)


listUsers :: Connection -> Int -> ExceptT ApiError IO (U.PaginatedResponse [U.ListUsersRow])
listUsers conn startId = do
    r <- liftIO $ (query conn [sql|
        select id, username
        from "user" where id > ? order by id limit 50
        |] (Only startId) :: IO [U.ListUsersRow])
    let next = case r of
                   [] -> ""
                   l -> show (maximum (map U._listUserRowId r))
    return (U.PaginatedResponse r next)


--example = do
--    c <- connect (defaultConnectInfo  { connectDatabase = "holborn", connectUser = "tom"})
--    runExceptT $ listUsers c 0
-- λ  pwd <- newPassword "hello"
-- λ  runExceptT $ signup c (newUsername "tom") (newEmail "x@y.com") pwd
-- Right ()
