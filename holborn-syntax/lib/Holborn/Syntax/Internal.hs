module Holborn.Syntax.Internal
       ( leftMergeBy
       ) where

import BasicPrelude


leftMergeBy :: (a -> b -> Bool) -> [a] -> [b] -> Either [b] [(a, Maybe b)]
leftMergeBy _ [] [] = return []
leftMergeBy _ [] ys = Left ys
leftMergeBy _ xs [] = return [(x, Nothing) | x <- xs]
leftMergeBy match (x:xs) allY@(y:ys) = do
  let (matched, ys') = if match x y then (Just y, ys) else (Nothing, allY)
  rest <- leftMergeBy match xs ys'
  return $ (x, matched):rest
