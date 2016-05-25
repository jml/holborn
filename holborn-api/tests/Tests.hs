{-# LANGUAGE RankNTypes #-}

module Main (main) where

import BasicPrelude

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Test.Tasty (defaultMain, TestTree, testGroup, withResource)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Holborn.API.Types (newPassword)

import Holborn.API.SSH (shellEncode)
import Holborn.JSON.SSHRepoCommunication (RepoCall)
import System.Process (shell, readCreateProcess)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, monitor, run)
import Test.Tasty.Hspec (testSpec, describe, it)
import Test.Hspec.Wai (with, get, shouldRespondWith)

import Holborn.API.Config (AppConf, Config(..), loadAppConf)
import Network.Wai (Application)
import Holborn.API (api, server)
import Servant (serve)
import System.Process (callCommand)


main :: IO ()
main = do
    waiTests <- waiTest
    defaultMain (testGroup "all" [waiTests, tests])


testApp :: IO Application
testApp = do
    callCommand "createuser holborn-test-user || true"
    callCommand "dropdb holborn-test-db || true"
    callCommand "createdb -O holborn-test-user holborn-test-db"
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
    print "Using config:"
    print conf
    appConf <- loadAppConf conf

    pure (serve api (server appConf))


waiTest :: IO TestTree
waiTest =
  testSpec "wai-tests" $ with (testApp) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200


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
