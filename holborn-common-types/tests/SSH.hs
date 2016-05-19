module SSH (tests) where

import BasicPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Holborn.JSON.SSHRepoCommunication"
  [ testProperty "a is a" $ \x -> x == (x :: Int)
  ]
