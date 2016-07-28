{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Entry point for the holborn-api test suite.

module Main (main) where

import HolbornPrelude

import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Internal
import qualified NewRepo
import qualified SSH

main :: IO ()
main = do
  allTests <- tests
  defaultMain allTests

tests :: IO TestTree
tests = do
  newRepoTests <- NewRepo.tests
  sshTests <- SSH.tests
  pure $ testGroup "Holborn.API"
         [ Internal.tests
         , newRepoTests
         , sshTests
         ]

-- TODO: Update the main app to only connect when we try to talk to the
-- database. Ideally, it should use a connection pool. See newPool and
-- withPoolConnection at
-- https://hackage.haskell.org/package/pgsql-simple-0.1.2/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo
