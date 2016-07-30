{-# LANGUAGE RankNTypes #-}

-- | Utilities for integrating with our test frameworks to provide resources
-- necessary for testing.

module Fixtures
  ( dbConfig
  , makeTestApp
  , testConfig
  , withConfig
  , withDatabaseResource
  ) where

import HolbornPrelude
import Paths_holborn_api (getDataFileName)

import Database.PostgreSQL.Simple (defaultConnectInfo)
import Network.Wai (Application)
import qualified Network.Wai.Middleware.RequestLogger as RL
import Test.Tasty (TestTree, withResource)
import Test.Tasty.Hspec (Spec, SpecWith, afterAll, aroundWith, beforeAll)

import Holborn.API (app)
import Holborn.API.Config (Config(..))

import Postgres (Postgres, connection, makeDatabase, resetPostgres, stopPostgres)


-- | Set this to True if you want the request logs from the api server to be
-- printed to stdout during tests.
showRequestLog :: Bool
showRequestLog = False

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

makeTestApp :: Config -> Application
makeTestApp = if showRequestLog then RL.logStdoutDev . app else app

dbConfig :: Postgres -> Config
dbConfig postgres = testConfig { dbConnection = connection postgres }

-- | Provide a Spec with everything it needs to have a valid Config object
-- pointing to a clean database instance.
withConfig :: SpecWith Config -> Spec
withConfig x = beforeAll makeHolbornDB $ afterAll stopPostgres $ aroundWith withConfig' $ x

-- | Turn an action that uses Config into an action that needs our
-- internal postgres storage variable.
withConfig' :: MonadIO m => (Config -> m a) -> Postgres -> m a
withConfig' action postgres = do
  liftIO $ resetPostgres postgres
  action (dbConfig postgres)

-- | Provide a "resource" for Tasty tests that loads a live database with a
-- clean schema.
withDatabaseResource :: (IO Postgres -> TestTree) -> TestTree
withDatabaseResource = withResource makeHolbornDB stopPostgres
