module Main (main) where

import HolbornPrelude
import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified RepoMeta
import qualified SSH
import qualified Types


tests :: TestTree
tests =
  testGroup "holborn-common-types"
  [ SSH.tests
  , RepoMeta.tests
  , Types.tests
  ]


main :: IO ()
main = defaultMain tests
