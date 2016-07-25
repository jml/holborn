{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Fixtures
  ( User(..)
  , dbConfig
  , makeApp
  , makeArbitraryUser
  , testConfig
  , withConfig
  , withDatabaseResource
  ) where

import HolbornPrelude
import Paths_holborn_api (getDataFileName)

import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (fromJust)
import Database.PostgreSQL.Simple (defaultConnectInfo)
import Network.Wai (Application)
import Servant (serve)
import Test.Tasty (TestTree, withResource)
import Test.Tasty.Hspec (Spec, SpecWith, afterAll, aroundWith, beforeAll)

import Holborn.API (api, server)
import Holborn.API.Auth (UserId)
import Holborn.API.Config (Config(..))
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

import Helpers (Postgres, connection, makeDatabase, stopPostgres)


-- | Create a blank instance of the Holborn database from scratch.
--
-- Will initialize Postgres, launch it, create a user, create a database, and
-- load the Holborn schema into the database.
makeHolbornDB :: IO Postgres
makeHolbornDB = do
  -- See https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
  schemaFile <- getDataFileName "sql/initial.sql"
  makeDatabase schemaFile

-- TODO: Figure out some way of properly getting a port for holborn-api while
-- also having correctly set base url.

-- TODO: Figure out a way of resetting the database to the schema between
-- tests.
-- https://www.postgresql.org/docs/9.5/static/manage-ag-templatedbs.html

-- | A value of Config that can be used in tests that don't *actually* access
-- the configuration for anything.
testConfig :: Config
testConfig = Config
  { port = 9999
  , dbConnection = defaultConnectInfo
  , configRepoHostname = ""
  , configRepoPort = 0
  , configRawRepoPort = 0
  }

dbConfig :: Postgres -> Config
dbConfig postgres = testConfig { dbConnection = connection postgres }

-- | Make an instance of the Holborn API suitable for use in tests.
makeApp :: Config -> Application
makeApp config = serve api (server config)

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

-- | Provide a Spec with everything it needs to have a valid Config object
-- pointing to a clean database instance.
withConfig :: SpecWith Config -> Spec
withConfig x = beforeAll makeHolbornDB $ afterAll stopPostgres $ aroundWith withConfig' $ x

-- | Turn an action that uses Config into an action that needs our
-- internal postgres storage variable.
withConfig' :: (Config -> a) -> Postgres -> a
withConfig' action postgres =
  -- TODO: This is the point where we should reset the database.
  action (dbConfig postgres)

-- XXX: jml has forgotten when this gets loaded & shut down etc. Important
-- because maybe we can use it for Hspec tests and have less duplication.
-- | Provide a "resource" for Tasty tests that loads a live database with a
-- clean schema.
withDatabaseResource :: (IO Postgres -> TestTree) -> TestTree
withDatabaseResource = withResource makeHolbornDB stopPostgres
