module RepoMeta (tests) where

import HolbornPrelude

import Helpers (jsonIdentity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Holborn.JSON.RepoMeta (RepoName)


tests :: TestTree
tests =
  testGroup "Holborn.JSON.RepoMeta"
  [ testGroup "RepoName"
    [ testProperty "To JSON and back" $ \x -> jsonIdentity (x :: RepoName)
    ]
  ]
