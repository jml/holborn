-- | Tests for Holborn.API.NewRepo

module NewRepo (spec) where

import HolbornPrelude

import Data.Aeson (object, (.=))
import Test.Hspec.Wai.Internal (withApplication)
import Test.Tasty.Hspec (SpecWith, describe, it)

import Holborn.API.Config (Config)

import Fixtures (makeTestApp)
import Helpers
  ( User(..)
  , makeArbitraryUser
  , postAs
  , respondsWithJSON
  )

spec :: SpecWith Config
spec = do
  describe "/v1/new-repo" $ do
    it "creates repo when posted to" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        let repoName = "name" :: Text
        postAs user "/v1/new-repo"
          (object [ "owner" .= (userName user)
                  , "name" .= repoName
                  , "description" .= ("description" :: Text)
                  , "private" .= False
                  , "initialize" .= False
                  ])
          `respondsWithJSON`
          (object [ "number_objects" .= (0 :: Int)
                  , "size" .= (0 :: Int)
                  , "owner" .= (userName user)
                  , "id" .= (1 :: Int)
                  , "number_commits" .= (0 :: Int)
                  ])
