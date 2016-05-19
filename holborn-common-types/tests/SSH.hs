{-# OPTIONS_GHC -fno-warn-orphans #-}

module SSH (tests) where

import BasicPrelude
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck ((===), Arbitrary(..), Property, elements, testProperty, Gen, listOf1)

import Holborn.JSON.SSHRepoCommunication
  ( SSHCommandLine(..)
  , unparseSSHCommand
  , parseSSHCommand
  )


instance Arbitrary Text where
  arbitrary = fromString <$> arbitrary


-- | Generate a valid path segment. Used for generating valid owners and repo
-- names.
pathSegment :: Gen Text
pathSegment = fromString <$> listOf1 (elements alphabet)
  where
    alphabet = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> "-_."


instance Arbitrary SSHCommandLine where
  arbitrary =
    constructor <*> pathSegment <*> pathSegment
    where constructor = elements [ GitReceivePack, GitUploadPack ]


jsonIdentity :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
jsonIdentity x = Just x === decode (encode x)


tests :: TestTree
tests =
  testGroup "Holborn.JSON.SSHRepoCommunication"
  [ testProperty "a is a" $ \x -> x == (x :: Int)
  , testGroup "SSHCommand properties"
    [ testProperty "unparsed then parsed" $ \x -> Just x === parseSSHCommand (unparseSSHCommand x)
    , testProperty "to JSON and back" $ \x -> jsonIdentity (x :: SSHCommandLine)
    ]
  ]
