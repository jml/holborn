module Main (main) where

import HolbornPrelude

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, (===))
import qualified Holborn.Repo.Search as S
import Data.Text.Arbitrary ()

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup "Holborn.Repo.Search"
  [ testProperty "all strings parse into a valid search" $ \x -> S.parseSearch x `seq` True === True
  ]
