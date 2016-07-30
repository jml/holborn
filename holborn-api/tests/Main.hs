{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Entry point for the holborn-api test suite.

module Main (main) where

import HolbornPrelude

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import Fixtures (withConfig)

import qualified Internal
import qualified NewRepo
import qualified Settings.SSHKeys
import qualified SSH


main :: IO ()
main = do
  allTests <- tests
  defaultMain allTests

tests :: IO TestTree
tests = do
  apiTests <- waiTests
  pure $ testGroup "Holborn.API"
         [ Internal.tests
         , apiTests
         ]

waiTests :: IO TestTree
waiTests = do
  testSpec "REST API tests" $ withConfig $ do
    NewRepo.spec
    Settings.SSHKeys.spec
    SSH.spec

-- TODO: Become very clever and figure out how to share a resource between
-- Tasty resources and Hspec tests. Would shave off ~3s from test runs on
-- jml's laptop.

-- TODO: Update the main app to only connect when we try to talk to the
-- database. Ideally, it should use a connection pool. See newPool and
-- withPoolConnection at
-- https://hackage.haskell.org/package/pgsql-simple-0.1.2/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo
