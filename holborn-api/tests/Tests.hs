{-# LANGUAGE RankNTypes #-}

module Main (main) where

import BasicPrelude

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Holborn.API.Types (newPassword)

import Holborn.API.SSH (shellEncode, shellQuote)
import Holborn.JSON.SSHRepoCommunication (RepoCall)
import System.Process (shell, readCreateProcess)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, monitor, run)


main :: IO ()
main = defaultMain tests


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
  , testProperty "shellQuote roundtrips through shell" $ \x -> ('\NUL' `notElem` x) ==>  monadicIO $ do
        output <- roundtripViaShell (textToString (shellQuote (fromString x)))
        assert (output == x)
  ]
