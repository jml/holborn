module SSH (tests) where

import BasicPrelude
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@=?)
  , testCase
  )
import Test.Tasty.QuickCheck
  ( (===)
  , Property
  , testProperty
  )

import Holborn.JSON.SSHRepoCommunication
  ( RepoCall(..)
  , SSHCommandLine(..)
  , unparseSSHCommand
  , parseSSHCommand
  )
import Holborn.JSON.RepoMeta (newValidRepoName)


jsonIdentity :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
jsonIdentity x = Just x === decode (encode x)

tests :: TestTree
tests =
  testGroup "Holborn.JSON.SSHRepoCommunication"
  [ testProperty "a is a" $ \x -> x == (x :: Int)
  , testGroup "SSHCommand"
    [ testProperty "unparsed then parsed" $ \x -> Just x === parseSSHCommand (unparseSSHCommand x)
    , testProperty "to JSON and back" $ \x -> jsonIdentity (x :: SSHCommandLine)
    , testCase "standard unparse example" $
      "git-upload-pack 'org/hello'" @=? unparseSSHCommand (GitUploadPack "org" validRepoName)
    , testCase "standard parse example" $
      Just (GitUploadPack "org" validRepoName) @=? parseSSHCommand "git-upload-pack 'org/hello'"
    ]
  , testGroup "RepoCall"
    [ testProperty "to JSON and back" $ \x -> jsonIdentity (x :: RepoCall)
    ]
  ]
  where
    Just validRepoName = newValidRepoName "hello"
