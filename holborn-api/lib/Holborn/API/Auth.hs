{-# LANGUAGE QuasiQuotes #-}

module Holborn.API.Auth
  ( UserId
  , getUserId
  ) where

import HolbornPrelude

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only(..))

import Holborn.API.Internal (APIError(..), APIHandler, throwAPIError, query)
import Holborn.API.Types (DexMail)


type UserId = Int


-- | Get the UserId for Username, ensuring that they are logged in.
--
-- Returns MissingAuthToken or InvalidAuthToken error (via ExceptT) otherwise.
getUserId :: Maybe DexMail -> APIHandler a UserId
getUserId Nothing = throwAPIError MissingAuthToken
getUserId (Just dexMail) = do
    rows <- query [sql|
        select id from "auth_user" where email = ?
    |] (Only dexMail)
    case rows of
      [Only userId] -> pure userId
      [] -> throwAPIError NoUserAccount
      _ -> throwAPIError InvalidAuthToken
