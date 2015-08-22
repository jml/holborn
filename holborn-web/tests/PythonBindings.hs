module PythonBindings where

import BasicPrelude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Holborn.Python (annotateSourceCode)
import Holborn.Scope (Annotation(..), ID)


pythonInput :: Text
pythonInput =
  unlines [ "x = 2"
          , "def f(y):"
          , "  return x + y"
          , "f(3)"
          ]


expectedOutput :: [(String, Maybe (Annotation ID))]
expectedOutput =
  [ ("x", Just (Binding 1))
  , ("=", Nothing)
  , ("2", Nothing)
  , ("def", Nothing)
  , ("f", Just (Binding 2))
  , ("(", Nothing)
  , ("y", Just (Binding 3))
  , (")", Nothing)
  , (":", Nothing)
  , ("return", Nothing)
  , ("x", Just (Reference 1))
  , ("+", Nothing) -- maybe actually a reference?
  , ("y", Just (Reference 3))
  , ("f", Just (Reference 2))
  , ("(", Nothing)
  , ("3", Nothing)
  , (")", Nothing)
  ]


tests :: TestTree
tests =
  testGroup "scope tests"
  [ testCase "very simple example" $
    Right expectedOutput @=? annotateSourceCode pythonInput
  ]
