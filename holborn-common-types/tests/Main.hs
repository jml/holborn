module Main (main) where

import BasicPrelude
import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified SSH


tests :: TestTree
tests =
  testGroup "holborn-common-types"
  [ SSH.tests ]


main :: IO ()
main = defaultMain tests
