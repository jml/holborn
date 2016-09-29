-- | Tests for Holborn.API.Internal

module Internal (tests) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Network.HTTP.Client (HttpException(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit hiding (assert)

import Holborn.API.Internal
  ( APIError(..)
  , APIHandler
  , rjsonGet'
  , runAPIHandler
  )

import Fixtures
  ( dbConfig
  , withDatabaseResource
  )
import Postgres (Postgres)


tests :: TestTree
tests = testGroup "Holborn.API.Internal"
        [ withDatabaseResource jsonGetTests ]


jsonGetTests :: IO Postgres -> TestTree
jsonGetTests getDB =
  testGroup "rjsonGet'"
  [ testCase "Bad URL fails in ExceptT" $ do
      config <- dbConfig <$> getDB
      let badUrl = "413213243214"
      let apiResult = (rjsonGet' (fromString badUrl) :: APIHandler Int (Either String Int))
      let expectedException = UnexpectedException (toException (InvalidUrlException badUrl "Invalid URL")) :: APIError Int
      result <- runExceptT (runAPIHandler config apiResult)
      case result of
        Left e -> show expectedException @?= (show e :: Text)
        Right _ -> assertFailure $ "Unexpectedly parsed URL: " ++ badUrl
  ]
