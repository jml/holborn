module Holborn.Internal
       ( leftMergeBy
       , FileType(..)
       , getFileType
       ) where

import BasicPrelude
import Control.Applicative (Alternative(..))

import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        )


leftMergeBy :: (a -> b -> Bool) -> [a] -> [b] -> Either [b] [(a, Maybe b)]
leftMergeBy _ [] [] = return []
leftMergeBy _ [] ys = Left ys
leftMergeBy _ xs [] = return [(x, Nothing) | x <- xs]
leftMergeBy match (x:xs) allY@(y:ys) = do
  let (matched, ys') = if match x y then (Just y, ys) else (Nothing, allY)
  rest <- leftMergeBy match xs ys'
  return $ (x, matched):rest


data FileType = File | Directory


getFileType :: (MonadIO m, Alternative a) => FilePath -> m (a FileType)
getFileType path = do
  isDir <- liftIO $ doesDirectoryExist path
  if isDir
    then return (pure Directory)
    else do
      isFile <- liftIO $ doesFileExist path
      if isFile
        then return (pure File)
        else return (Control.Applicative.empty)