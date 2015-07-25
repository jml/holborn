import BasicPrelude

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "hello" [
    testCase "test true" $ 1 @?= 1
    ]
