module SSH (tests) where

import HolbornPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@=?)
  , testCase
  )
import Test.Tasty.QuickCheck
  ( (===)
  , testProperty
  )
import Holborn.JSON.SSHRepoCommunication
  ( GitCommand(..)
  , RepoCall(..)
  , SSHCommandLine(..)
  , unparseSSHCommand
  , parseSSHCommand
  )
import Holborn.JSON.RepoMeta (newOwnerName, newRepoName)
import Helpers (jsonIdentity, httpApiDataIdentity)

tests :: TestTree
tests =
  testGroup "Holborn.JSON.SSHRepoCommunication"
  [ testProperty "a is a" $ \x -> x == (x :: Int)
  , testGroup "SSHCommand"
    [ testProperty "unparsed then parsed" $ \x -> Just x === parseSSHCommand (unparseSSHCommand x)
    , testProperty "to JSON and back" $ \x -> jsonIdentity (x :: SSHCommandLine)
    , testCase "standard unparse example" $
      "git-upload-pack 'org/hello'" @=? unparseSSHCommand (SSHCommandLine GitUploadPack validOrgName validRepoName)
    , testCase "standard parse example" $
      Just (SSHCommandLine GitUploadPack validOrgName validRepoName) @=? parseSSHCommand "git-upload-pack 'org/hello'"
    ]
  , testGroup "GitCommand"
    [ testProperty "unparsed then parsed" $ \x -> httpApiDataIdentity (x :: GitCommand)
    ]
  , testGroup "RepoCall"
    [ testProperty "to JSON and back" $ \x -> jsonIdentity (x :: RepoCall)
    ]
  ]
  where
    Just validOrgName = newOwnerName "org"
    Just validRepoName = newRepoName "hello"
