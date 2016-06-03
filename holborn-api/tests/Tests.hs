{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import BasicPrelude

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Holborn.API (api, server)
import Holborn.API.Config (AppConf, Config(..), loadAppConf)
import Holborn.API.SSH (shellEncode)
import Holborn.API.Types (newPassword)
import Holborn.JSON.SSHRepoCommunication (RepoCall)
import qualified Network.HTTP.Types.Method as Method
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


main :: IO ()
main = do
    defaultMain (testGroup "all" [tests])


resetDB :: IO ()
resetDB = do
    callCommand "createuser holborn-test-user || true"
    callCommand "dropdb holborn-test-db || true"
    callCommand "createdb -O holborn-test-user holborn-test-db"


testApp :: IO Application
testApp = do
    callCommand "psql -q -f sql/initial.sql holborn-test-db -U holborn-test-user"
    callCommand "psql -q -f sql/sample-data.sql holborn-test-db -U holborn-test-user"
    let conf = Config
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
    appConf <- loadAppConf conf
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
