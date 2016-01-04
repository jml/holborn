{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
module Holborn.UI.User
       ( signup
       , listUsers
       ) where

import BasicPrelude
import Database.PostgreSQL.Simple (execute, Connection, query, Only(..))
import Database.PostgreSQL.Simple.Errors (constraintViolation)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.UI.Types (Email, Password, Username, ApiError(..))
import Control.Error (ExceptT, throwE)
import Data.Aeson (ToJSON)

import qualified Holborn.Models.Users as U

signup :: Connection -> Username -> Email -> Password -> ExceptT ApiError IO ()
signup conn u e p = do
    res <- catchJust constraintViolation
        (liftIO (execute conn [sql|
            insert into "user" (id, username, signup_email, password, created)
            values (default, ?, ?, ?, default)
            |] (u, e, p)))
        (\_ -> (throwE (UserAlreadyExists u)))
    return ()


listUsers :: Connection -> Int -> ExceptT ApiError IO [U.ListUsersRow]
listUsers conn startId = do
    r <- liftIO $ (query conn [sql|
        select id, username
        from "user" where id > ? limit 50
        |] (Only startId) :: IO [U.ListUsersRow])
    return r


--example = do
--    c <- connect (defaultConnectInfo  { connectDatabase = "holborn", connectUser = "tom"})
--    runExceptT $ listUsers c 0
-- λ  pwd <- newPassword "hello"
-- λ  runExceptT $ signup c (newUsername "tom") (newEmail "x@y.com") pwd
-- Right ()
