import BasicPrelude

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.QuickCheck

import Holborn.Web (leftMergeBy)


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
tests = testGroup "leftMerge" [
  testProperty "allLeftsInResult" $ \(xs, ys) -> allLeftsInResult (==) xs (ys :: [Int])
  , testProperty "identicalLists" $ \xs -> identicalLists (xs :: [Int])
  , testProperty "onlyLefts" $ \xs -> onlyLefts (xs :: [Int])
  , testProperty "onlyRights" $ \xs -> onlyRights (xs :: [Int])
  ]


main :: IO ()
main = defaultMain tests
