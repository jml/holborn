{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Database.PostgreSQL.Simple (defaultConnectInfo)
import qualified Network.HTTP.Types.Method as Method
import Network.HTTP.Client.Internal (HttpException(..))
import Network.Wai (Application)
import Servant (serve)
import System.Process (callCommand)
import Test.Hspec.Wai (with, request, shouldRespondWith, ResponseMatcher(..))
import Test.Hspec.Wai.JSON (json)
import Test.Tasty (defaultMain, TestTree, testGroup, withResource)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hspec (testSpec, describe, it)

import Holborn.API (api, server)
import Holborn.API.Config (Config(..))
import Holborn.API.Internal (APIError(..), APIHandler, jsonGet', runAPIHandler)
import Holborn.API.Types (newPassword)

import Helpers (Postgres, connection, makeDatabase, stopPostgres)

main :: IO ()
main = do
  tests <- suite
  defaultMain tests


suite :: IO TestTree
suite = do
  pure $ testGroup "Holborn.API"
         [ simpleTests
         , withResource (makeDatabase "sql/initial.sql") stopPostgres apiTests
         ]

-- TODO: Use Helpers here to make postgres (withResource!)

-- TODO: Figure out some way of properly getting a port for holborn-api while
-- also having correctly set base url.

-- TODO: Figure out a way of resetting the database to the schema between
-- tests.

-- TODO: Update the main app to only connect when we try to talk to the
-- database. Ideally, it should use a connection pool. See newPool and
-- withPoolConnection at
-- https://hackage.haskell.org/package/pgsql-simple-0.1.2/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo

-- TODO: Figure out how to refer to schema.sql safely

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


testApp :: IO Application
testApp = do
    callCommand "psql -q -f sql/initial.sql holborn-test-db -U holborn-test-user"
    callCommand "psql -q -f sql/sample-data.sql holborn-test-db -U holborn-test-user"
    pure (serve api (server testConfig))


authenticatedPost path body = request Method.methodPost path [("GAP-Auth", "alice"), ("content-type", "application/json")] body

waiTest :: IO TestTree
waiTest = do
  testSpec "wai-tests" $ with testApp $ do
    describe "the new-repo endpoint" $ do
        it "creates repo when posted to" $ do
            authenticatedPost "/v1/new-repo"
              [json|{owner: "alice", name: "name", description: "", private: false, initialize: false}|]
              `shouldRespondWith`
              [json|{number_objects:0,size:0,owner:"alice",repo:"name",number_commits:0}|]
              {matchStatus = 200}


apiTests :: IO Postgres -> TestTree
apiTests getDB =
  testGroup "Holborn.API integration tests"
  [ testCase "Bad URL fails in ExceptT" $ do
      postgres <- getDB
      let config = testConfig { dbConnection = connection postgres }
      let badUrl = "413213243214"
      let apiResult = (jsonGet' (fromString badUrl) :: APIHandler Int (Either String Int))
      let expectedException = UnexpectedException (toException (InvalidUrlException badUrl "Invalid URL")) :: APIError Int
      result <- runExceptT (runAPIHandler config apiResult)
      case result of
        Left e -> show expectedException @?= show e
        Right _ -> assertFailure $ "Unexpectedly parsed URL: " ++ badUrl
  ]


simpleTests :: TestTree
simpleTests =
  testGroup "Simple tests"
  [ testCase "password not shown" $ do
        pwd <- newPassword "hello"
        show pwd @?= "*hidden-password*"
  ]
