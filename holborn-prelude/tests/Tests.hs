module Main (main) where

import HolbornPrelude
import Test.Tasty (TestTree, defaultMain, testGroup)


tests :: TestTree
tests = testGroup "HolbornPrelude" []

main :: IO ()
main = defaultMain tests
