module Types (tests) where

import HolbornPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@?=)
  , testCase
  )

import Holborn.API.Types (newPassword)

tests :: TestTree
tests =
  testGroup "Holborn.API.Types"
  [ testCase "password not shown" $ do
        pwd <- newPassword "hello"
        show pwd @?= "*hidden-password*"
  ]
