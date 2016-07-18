module Main (main) where

import HolbornPrelude
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit hiding (assert)


main :: IO ()
main = defaultMain nothingTest

nothingTest :: TestTree
nothingTest =
  testGroup "Holborn.Proxy"
  [ testCase "1 == 1" $ do
        1 @?= (1 :: Int)
  ]
