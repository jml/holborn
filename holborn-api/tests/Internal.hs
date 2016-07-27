-- | Tests for Holborn.API.Internal

module Internal (suite) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Network.HTTP.Client (HttpException(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit hiding (assert)

import Holborn.API.Internal
  ( APIError(..)
  , APIHandler
  , jsonGet'
  , runAPIHandler
  )

import Fixtures
  ( dbConfig
  , withDatabaseResource
  )
import Postgres (Postgres)


suite :: TestTree
suite = testGroup "Holborn.API.Internal"
        [ withDatabaseResource jsonGetTests ]


jsonGetTests :: IO Postgres -> TestTree
jsonGetTests getDB =
  testGroup "jsonGet'"
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
