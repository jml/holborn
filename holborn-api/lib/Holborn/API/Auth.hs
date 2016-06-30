{-# LANGUAGE QuasiQuotes #-}

module Holborn.API.Auth
  ( getUserId
  ) where

import HolbornPrelude

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only(..))

import Holborn.API.Internal (APIError(..), APIHandler, throwAPIError, query)
import Holborn.API.Types (Username)


type UserId = Int


-- | Get the UserId for Username, ensuring that they are logged in.
--
-- Returns MissingAuthToken or InvalidAuthToken error (via ExceptT) otherwise.
getUserId :: Maybe Username -> APIHandler a UserId
getUserId Nothing = throwAPIError MissingAuthToken
getUserId (Just username) = do
    -- TODO: FAKE: Hardcodes email@test
    rows <- query [sql|
        insert into "user"  (username, email) select ?, 'email@test' where not exists (select 1 from "user" where username = ?);
        select id from "user" where username = ?
    |] (username, username, username)
    case rows of
      [Only userId] -> pure userId
      _ -> throwAPIError InvalidAuthToken
