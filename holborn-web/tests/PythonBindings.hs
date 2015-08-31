{-|
Tests for the Python binding and reference logic.
|-}
module PythonBindings (tests) where

import BasicPrelude
import Control.Applicative (Alternative)
import Data.Foldable (asum)

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit

import Holborn.Python (annotateSourceCode)
import Holborn.Scope (ID)
import Holborn.Types (Annotation(..))


-- These look weird, but they allow us a relatively succinct way of expressing
-- the desired output from the analyzer.

-- XXX: It could be better. For a start, it's too ugly and too hard to read.
-- Second, the `where` clauses mean that we can't embed them in the top-level
-- test tree object, which means it's really easy to write a test but not add
-- it to the global runner, which in turn can delude the author into thinking
-- their test passes when it does not.

-- | A stretch of tokens that don't have any bindings or references.
--
-- It's not a full-fledged tokenizer, so you must remember to separate tokens
-- with spaces. e.g.,
--
-- >>> n "print ( 3 + 2 )"
-- [("print", Nothing), ("(", Nothing), ("3", Nothing), ("+", Nothing),
--  ("2", Nothing), (")", Nothing)]
n :: Alternative m => Text -> m (String, Maybe b)
n xs = asum [pure (textToString x, Nothing) | x <- words xs]

-- | A binding.
--
-- >>> b "x" 3
-- ("x", Just (Binding 3))
b :: Applicative m => a -> b -> m (a, Maybe (Annotation b))
b x i = pure (x, Just (Binding i))

-- | A reference.
--
-- >>> r "x" 3
-- ("x", Just (Reference 3))
r :: Applicative m => a -> b -> m (a, Maybe (Annotation b))
r x i = pure (x, Just (Reference i))


-- | An unresolved reference.
--
-- >>> u "x"
-- ("x", Just UnresolvedReference)
u :: Alternative m => a -> m (a, Maybe (Annotation b))
u x = pure (x, Just UnresolvedReference)


-- | Helper for constructing tests.
testAST :: Foldable t => TestName  -- ^ The name of the test
        -> [Text]  -- ^ The input source code, where each line is an element of the list
        -> t [(String, Maybe (Annotation ID))]  -- ^ The expected output, build using 'n', 'b', and 'r'.
        -> TestTree  -- ^ The resulting test
testAST name input output =
  testCase name $ Right output' @=? annotateSourceCode' input
  where output' = asum output
        annotateSourceCode' = annotateSourceCode . unlines


-- | Simple exercise of the analyzer.
simpleTest :: TestTree
simpleTest =
  testAST "very simple example" input output
  where
    input =
      [ "x = 2"
      , "def f(y):"
      , "  return x + y"
      , "f(3)"
      ]
    output =
      [ b "x" 1 , n "= 2"
      , n "def" , b "f" 2, n "(" , b "y" 3, n ") :"
      , n "return" , r "x" 1, n "+" , r "y" 3
      , r "f" 2, n "( 3 )"
      ]


augmentedAssignment :: TestTree
augmentedAssignment =
  testAST "x += 2" input output
  where
    input =
      [ "x = 0"
      , "x += 1"
      ]
    output =
      [ b "x" 1, n "= 0"
      , r "x" 1, n "+= 1"
      ]


redefinition :: TestTree
redefinition =
  testAST "redefinition" input output
  where
    input =
      [ "x = 0"
      , "y = x + 1"
      , "x = 1"
      , "z = x + y"
      ]
    output =
      [ b "x" 1, n "= 0"
      , b "y" 2, n "=", r "x" 1, n "+ 1"
      , b "x" 3, n "= 1"
      , b "z" 4, n "=", r "x" 3, n "+", r "y" 2
      ]


whileLoop :: TestTree
whileLoop =
  testAST "while/else loop" input output
  where
    input =
      [ "while True:"
      , "  x = 1"
      , "else:"
      , "  print x"
      ]
    output =
      [ n "while",  u "True",  n ":"
      , b "x" 1, n "= 1"
      , n "else :"
      , n "print", r "x" 1
      ]


classDefinition :: TestTree
classDefinition =
  testAST "class definition" input output
  where
    input =
      [ "Bar = int"
      , "class Foo(Bar):"
      , "  pass"
      ]
    output =
      [ b "Bar" 1, n "=", u "int"
      , n "class", b "Foo" 2, n "(", r "Bar" 1, n ") :"
      , n "pass"
      ]

-- XXX: Add a thing to test methods

simpleImport :: TestTree
simpleImport =
  testAST "import foo" input output
  where
    input =
      [ "import foo"
      , "foo"
      ]
    output =
      [ n "import", b "foo" 1
      , r "foo" 1
      ]


aliasImport :: TestTree
aliasImport =
  testAST "import foo as bar" input output
  where
    input =
      [ "import foo as bar"
      , "foo"
      , "bar"
      ]
    output =
      [ n "import foo as", b "bar" 1
      , u "foo"
      , r "bar" 1
      ]


fromImport :: TestTree
fromImport =
  testAST "from foo import bar" input output
  where
    input =
      [ "from foo import bar"
      , "foo"
      , "bar"
      ]
    output =
      [ n "from foo import", b "bar" 1
      , u "foo"
      , r "bar" 1
      ]


fromImportAlias :: TestTree
fromImportAlias =
  testAST "from foo import bar as baz" input output
  where
    input =
      [ "from foo import bar as baz"
      , "foo"
      , "bar"
      , "baz"
      ]
    output =
      [ n "from foo import bar as", b "baz" 1
      , u "foo"
      , u "bar"
      , r "baz" 1
      ]


-- XXX: We do not yet handle dotted names.
decorated :: TestTree
decorated =
  testAST "decorators" input output
  where
    input =
      [ "foo = None"
      , "@foo"
      , "def f(x):"
      , "  print x"
      ]
    output =
      [ b "foo" 1, n "=", u "None"
      , n "at", r "foo" 1 -- XXX: Why is this "at" and not "@"?
      , n "def", b "f" 2, n "(", b "x" 3, n ") :"
      , n "print", r "x" 3
      ]


decoratedWithArgs :: TestTree
decoratedWithArgs =
  testAST "decorators" input output
  where
    input =
      [ "foo = None"
      , "bar = None"
      , "@foo(bar)"
      , "def f(x):"
      , "  print x"
      ]
    output =
      [ b "foo" 1, n "=", u "None"
      , b "bar" 2, n "=", u "None"
      , n "at", r "foo" 1, n "(", r "bar" 2, n ")"
      , n "def", b "f" 3, n "(", b "x" 4, n ") :"
      , n "print", r "x" 4
      ]


raise :: TestTree
raise =
  testAST "raise" input output
  where
    input =
      [ "foo = ValueError(1)"
      , "raise foo"
      ]
    output =
      [ b "foo" 1, n "=",  u "ValueError",  n"( 1 )"
      , n "raise", r "foo" 1
      ]


del :: TestTree
del =
  testAST "del" input output
  where
    input =
      [ "foo = 2"
      , "del foo"
--      , "print foo"
      ]
    output =
      [ b "foo" 1, n "= 2"
      , n "delete", r "foo" 1
--      , n "print", u "foo"
      ]


-- XXX: Add a test for nested functions: would be a handy proof-of-concept for
-- scope stuff.


tests :: TestTree
tests =
  testGroup "scope tests"
  [ simpleTest
  , augmentedAssignment
  , whileLoop
  , classDefinition
  , simpleImport
  , aliasImport
  , fromImport
  , fromImportAlias
  , decorated
  , raise
  , decoratedWithArgs
  , redefinition
  , del
  ]
