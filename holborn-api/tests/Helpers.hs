{-# LANGUAGE QuasiQuotes #-}

-- | Code for manipulating holborn-api that does not actually belong in
-- holborn-api.

module Helpers
  ( User(..)
  , makeArbitraryUser
  ) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (fromJust)

import Holborn.API.Auth (UserId)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( execute
  , runAPIHandler
  , sql
  )
import Holborn.API.Types
  ( Email
  , Username
  , newUsername
  , newEmail
  )

-- | A user we have created for testing.
--
-- Exists so we can provide easy APIs for writing tests without worrying too
-- much about impact on production code.
data User = User { _userId    :: UserId
                 , userName  :: Username
                 , _userEmail :: Email
                 } deriving (Eq, Show)


-- | Make an arbitrary user for testing.
makeArbitraryUser :: MonadIO m => Config -> m User
makeArbitraryUser config = do
  -- TODO: Make this actually arbitrary.
  -- TODO: Remove duplication between query & `User` construction.
  userid <- liftIO $ runExceptT $ runAPIHandler config $ execute [sql|insert into "user" (username, email) values (?, ?)|] ("alice" :: Text, "alice@example.com" :: Text)
  case userid of
    Left _ -> terror "Could not create user in database and jml too lazy/stupid to show proper error"
    Right userid' -> pure $ User (fromIntegral userid') username email
  where
    username = newUsername "alice"
    email = fromJust (newEmail "alice@example.com")
