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

import Postgres (Postgres, connection, makeDatabase, stopPostgres)


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
