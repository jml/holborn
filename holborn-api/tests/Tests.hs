{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import HolbornPrelude

import Data.Aeson (object, (.=))
import Test.Hspec.Wai (shouldRespondWith)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (fromValue)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (testSpec, describe, it)

import Fixtures
  ( makeTestApp
  , withConfig
  )
import Helpers
  ( User(..)
  , makeArbitraryUser
  , postAs
  )
import qualified Internal


main :: IO ()
main = do
  tests <- suite
  defaultMain tests


suite :: IO TestTree
suite = do
  waiTests <- waiTest
  pure $ testGroup "Holborn.API"
         [ Internal.suite
         , waiTests
         ]

-- TODO: Update the main app to only connect when we try to talk to the
-- database. Ideally, it should use a connection pool. See newPool and
-- withPoolConnection at
-- https://hackage.haskell.org/package/pgsql-simple-0.1.2/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo

waiTest :: IO TestTree
waiTest = do
  testSpec "wai-tests" $ withConfig $ do
    describe "/v1/new-repo" $ do
      it "creates repo when posted to" $ \config -> do
        user <- makeArbitraryUser config
        withApplication (makeTestApp config) $ do
          let repoName = "name" :: Text
          postAs user "/v1/new-repo"
            (fromValue $ object [ "owner" .= (userName user)
                                , "name" .= repoName
                                , "description" .= ("description" :: Text)
                                , "private" .= False
                                , "initialize" .= False
                                ])
            `shouldRespondWith`
            (fromValue $ object [ "number_objects" .= (0 :: Int)
                                , "size" .= (0 :: Int)
                                , "owner" .= (userName user)
                                , "id" .= (1 :: Int)
                                , "number_commits" .= (0 :: Int)
                                ])
