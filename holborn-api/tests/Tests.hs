{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (object, (.=))
import qualified Network.HTTP.Types.Method as Method
import Network.HTTP.Client.Internal (HttpException(..))
import Test.Hspec.Wai (request, shouldRespondWith)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (fromValue)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hspec (testSpec, describe, it)
import Web.HttpApiData (toHeader)

import Holborn.API (app)
import Holborn.API.Internal
  ( APIError(..)
  , APIHandler
  , jsonGet'
  , runAPIHandler
  )
import Holborn.API.Types (newPassword)

import Postgres (Postgres)
import Fixtures
  ( User(..)
  , dbConfig
  , makeArbitraryUser
  , withConfig
  , withDatabaseResource
  )

main :: IO ()
main = do
  tests <- suite
  defaultMain tests


suite :: IO TestTree
suite = do
  waiTests <- waiTest
  pure $ testGroup "Holborn.API"
         [ simpleTests
         , withDatabaseResource apiTests
         , waiTests
         ]

-- TODO: Update the main app to only connect when we try to talk to the
-- database. Ideally, it should use a connection pool. See newPool and
-- withPoolConnection at
-- https://hackage.haskell.org/package/pgsql-simple-0.1.2/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo

waiTest :: IO TestTree
waiTest = do
  testSpec "wai-tests" $ withConfig $ do
    describe "/v1/new-repo" $ do
      it "creates repo when posted to" $ \config -> do
        user <- makeArbitraryUser config
        withApplication (app config) $ do
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


apiTests :: IO Postgres -> TestTree
apiTests getDB =
  testGroup "Holborn.API integration tests"
  [ testCase "Bad URL fails in ExceptT" $ do
      config <- dbConfig <$> getDB
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
