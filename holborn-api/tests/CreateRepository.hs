module CreateRepository (spec) where

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
  describe "/v1/create-repository" $ do
    it "creates repo when posted to" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        let repoName = "name" :: Text
        postAs user "/v1/create-repository"
          (object [ "owner" .= (userName user)
                  , "name" .= repoName
                  , "description" .= ("description" :: Text)
                  , "visibility" .= ("public" :: Text)
                  ])
          `respondsWithJSON`
          (object [ "id" .=  (1 :: Int)
                  ])
