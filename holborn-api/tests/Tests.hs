{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import qualified Network.HTTP.Types.Method as Method
import Network.HTTP.Client.Internal (HttpException(..))
import Network.Wai (Application)
import Servant (serve)
import System.Process (callCommand, shell, readCreateProcess)
import Test.Hspec.Wai (with, request, shouldRespondWith, ResponseMatcher(..))
import Test.Hspec.Wai.JSON (json)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, monitor, run)
import Test.Tasty (defaultMain, TestTree, testGroup, withResource)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hspec (testSpec, describe, it)
import Test.Tasty.QuickCheck

import Holborn.API (api, server)
import Holborn.API.Config (AppConf, Config(..), loadAppConf)
import Holborn.API.Internal (APIError(..), APIHandler, jsonGet', runAPIHandler)
import Holborn.API.SSH (shellEncode)
import Holborn.API.Types (newPassword)
import Holborn.JSON.SSHRepoCommunication (RepoCall)


main :: IO ()
main = do
    defaultMain (testGroup "all" [tests])


resetDB :: IO ()
resetDB = do
    callCommand "dropdb holborn-test-db || true"
    callCommand "createdb -O holborn-test-user holborn-test-db"

-- | A value of Config that can be used in tests that don't *actually* access
-- the configuration for anything.
testAppConf :: IO AppConf
testAppConf = loadAppConf $ Config
  { port = 9999
  , pgDb = "holborn-test-db"
  , pgUser = "holborn-test-user"
  , pgPort = 5432
  , configBaseUrl = "http://127.0.0.1:9999/"
  , configStaticBaseUrl = ""
  , configRepoHostname = ""
  , configRepoPort = 0
  , configRawRepoHostname = ""
  , configRawRepoPort = 0
  }


testApp :: IO Application
testApp = do
    callCommand "psql -q -f sql/initial.sql holborn-test-db -U holborn-test-user"
    callCommand "psql -q -f sql/sample-data.sql holborn-test-db -U holborn-test-user"
    appConf <- testAppConf
    pure (serve api (server appConf))


authenticatedPost path body = request Method.methodPost path [("GAP-Auth", "alice"), ("content-type", "application/json")] body

waiTest :: IO TestTree
waiTest = do
  resetDB
  testSpec "wai-tests" $ with testApp $ do
    describe "the new-repo endpoint" $ do
        it "creates repo when posted to" $ do
            authenticatedPost "/v1/new-repo"
              [json|{owner: "alice", name: "name", description: "", private: false, initialize: false}|]
              `shouldRespondWith`
              [json|{number_objects:0,size:0,owner:"alice",repo:"name",number_commits:0}|]
              {matchStatus = 200}


apiTests :: TestTree
apiTests =
  testGroup "Holborn.API integration tests"
  [ testCase "Bad URL fails in ExceptT" $ do
        resetDB
        let badUrl = "413213243214"
        let apiResult = (jsonGet' (fromString badUrl) :: APIHandler Int (Either String Int))
        let expectedException = UnexpectedException (toException (InvalidUrlException badUrl "Invalid URL")) :: APIError Int
        config <- testAppConf
        result <- runExceptT (runAPIHandler config apiResult)
        case result of
          Left e -> show expectedException @?= show e
          Right _ -> assertFailure $ "Unexpectedly parsed URL: " ++ badUrl
  ]


stringToBytes :: String -> LByteString
stringToBytes = fromStrict . encodeUtf8 . fromString


roundtripViaShell :: String -> PropertyM IO String
roundtripViaShell input = do
  output <- run (readCreateProcess (shell $ "echo -n " <> input) "")
  monitor (counterexample ("output: " <> output))
  return output


-- | A given object can be encoded to JSON, echoed via the shell, and then
-- decoded to get the original object back.
prop_roundTripsViaShell :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> PropertyM IO ()
prop_roundTripsViaShell input = do
  output <- roundtripViaShell (textToString (shellEncode input))
  let parsed = decode (stringToBytes output)
  monitor (counterexample ("parsed: " <> textToString (show parsed)))
  assert (parsed == Just input)


tests :: TestTree
tests =
  testGroup "Holborn.API"
  [ testCase "password not shown" $ do
        pwd <- newPassword "hello"
        show pwd @?= "*hidden-password*"
  , testProperty "repo call roundtrips through shell" $ \x ->
        monadicIO $ prop_roundTripsViaShell (x :: RepoCall)
  ]
