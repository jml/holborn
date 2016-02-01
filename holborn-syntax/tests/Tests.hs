import BasicPrelude

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Holborn.Syntax.Internal (leftMergeBy)

import qualified PythonBindings


allLeftsInResult :: Eq a => (a -> b -> Bool) -> [a] -> [b] -> Bool
allLeftsInResult f xs ys =
  case leftMergeBy f xs ys of
    Left _ -> True
    Right merged -> [ x | (x, _) <- merged ] == xs


identicalLists :: Eq a => [a] -> Bool
identicalLists xs = leftMergeBy (==) xs xs == Right [(x, Just x) | x <- xs]


onlyLefts :: Eq a => [a] -> Bool
onlyLefts xs = leftMergeBy (==) xs [] == Right [(x, Nothing) | x <- xs]


onlyRights :: Eq a => [a] -> Property
onlyRights ys = not (null ys) ==> leftMergeBy (==) [] ys == Left ys


tests :: TestTree
tests =
  testGroup "Holborn tests"
  [ testGroup "Web tests"
    [ testGroup "leftMerge"
      [ testProperty "allLeftsInResult" $ \(xs, ys) -> allLeftsInResult (==) xs (ys :: [Int])
      , testProperty "identicalLists" $ \xs -> identicalLists (xs :: [Int])
      , testProperty "onlyLefts" $ \xs -> onlyLefts (xs :: [Int])
      , testProperty "onlyRights" $ \xs -> onlyRights (xs :: [Int])
      ]
    , testGroup "leftMerge unit tests"
      [ testCase "worked example" $
        leftMergeBy (==) ([1, 2, 3, 4, 5] :: [Int]) [1, 3, 5]
        @?= Right [(1, Just 1), (2, Nothing), (3, Just 3), (4, Nothing), (5, Just 5)]
      ]
    ]
  , PythonBindings.tests
  ]


main :: IO ()
main = defaultMain tests
