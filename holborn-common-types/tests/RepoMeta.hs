module RepoMeta (tests) where

import HolbornPrelude

import Helpers (jsonIdentity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Holborn.CommonTypes.Repo (RepoName)


tests :: TestTree
tests =
  testGroup "Holborn.CommonTypes.Repo"
  [ testGroup "RepoName"
    [ testProperty "To JSON and back" $ \x -> jsonIdentity (x :: RepoName)
    ]
  ]
