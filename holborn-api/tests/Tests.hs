{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import HolbornPrelude
import Paths_holborn_api (getDataFileName)

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import Database.PostgreSQL.Simple (defaultConnectInfo)
import qualified Network.HTTP.Types.Method as Method
import Network.HTTP.Client.Internal (HttpException(..))
import Network.Wai (Application)
import Servant (serve)
import Test.Hspec.Wai (request, shouldRespondWith)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (fromValue)
import Test.Tasty (defaultMain, TestTree, testGroup, withResource)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hspec (Spec, SpecWith, afterAll, aroundWith, beforeAll, testSpec, describe, it)
import Web.HttpApiData (toHeader)

import Holborn.API (api, server)
import Holborn.API.Auth (UserId)
import Holborn.API.Config (Config(..))
import Holborn.API.Internal
  ( APIError(..)
  , APIHandler
  , execute
  , jsonGet'
  , runAPIHandler
  , sql
  )
import Holborn.API.Types
  ( Email
  , Username
  , newUsername
  , newPassword
  , newEmail
  )

import Helpers (Postgres, connection, makeDatabase, stopPostgres)

main :: IO ()
main = do
  tests <- suite
  defaultMain tests


suite :: IO TestTree
suite = do
  waiTests <- waiTest
  pure $ testGroup "Holborn.API"
         [ simpleTests
         , withResource (makeHolbornDB) stopPostgres apiTests
         , waiTests
         ]

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

-- TODO: Update the main app to only connect when we try to talk to the
-- database. Ideally, it should use a connection pool. See newPool and
-- withPoolConnection at
-- https://hackage.haskell.org/package/pgsql-simple-0.1.2/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo


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


testApp :: Config -> Application
testApp config = serve api (server config)


data User = User { _userId    :: UserId
                 , userName  :: Username
                 , _userEmail :: Email
                 } deriving (Eq, Show)


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


waiTest :: IO TestTree
waiTest = do
  testSpec "wai-tests" $ withConfig $ do
    describe "/v1/new-repo" $ do
      it "creates repo when posted to" $ \config -> do
        user <- makeArbitraryUser config
        withApplication (testApp config) $ do
          let repoName = "name" :: Text
          authenticatedPost user "/v1/new-repo"
            (fromValue $ object [ "owner" .= (userName user)
                                , "name" .= repoName
                                , "description" .= ("description" :: Text)
                                , "private" .= False
                                , "initialize" .= False
                                ])
            `shouldRespondWith`
            (fromValue $ object [ "number_objects" .= (0 :: Int)
                                , "size" .= (0 :: Int)
                                , "owner" .= (userName user)
                                , "id" .= (1 :: Int)
                                , "number_commits" .= (0 :: Int)
                                ])
  where
    authenticatedPost user path body =
      request Method.methodPost path [("GAP-Auth", (toHeader (userName user))), ("content-type", "application/json")] body

    -- | Provide a Spec with everything it needs to have a valid Config object
    -- pointing to a clean database instance.
    withConfig :: SpecWith Config -> Spec
    withConfig x = beforeAll makeHolbornDB $ afterAll stopPostgres $ aroundWith withConfig' $ x

    -- | Turn an action that uses Config into an action that needs our
    -- internal postgres storage variable.
    withConfig' action postgres = do
      let config = testConfig { dbConnection = connection postgres }
      -- TODO: This is the point where we should reset the database.
      action config


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
