-- | Tests for Holborn.API.NewRepo

module NewRepo (tests) where

import HolbornPrelude

import Data.Aeson (object, (.=))
import Test.Hspec.Wai (shouldRespondWith)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (fromValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (SpecWith, testSpec, describe, it)

import Holborn.API.Config (Config)

import Fixtures
  ( makeTestApp
  , withConfig
  )
import Helpers
  ( User(..)
  , makeArbitraryUser
  , postAs
  )

tests :: IO TestTree
tests = do
  sshSpec <- testSpec "/v1/new-repo" $ withConfig $ spec
  pure $ testGroup "Holborn.API.NewRepo" [ sshSpec ]


spec :: SpecWith Config
spec = do
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
