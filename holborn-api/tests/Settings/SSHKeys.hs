-- | Tests for Holborn.API.Settings.SSHKeys

module Settings.SSHKeys (spec) where

import HolbornPrelude

import Test.Tasty.Hspec (SpecWith, describe, it)
import Test.Hspec.Wai (get, shouldRespondWith)
import Test.Hspec.Wai.Internal (withApplication)

import Holborn.API.Config (Config)

import Fixtures (makeTestApp)

spec :: SpecWith Config
spec = do
  describe "/v1/users/<username>/keys" $
    it "Returns empty list for non-existent users" $ \config -> do
      withApplication (makeTestApp config) $ do
        -- Actually, a 404 would be more appropriate, but might as well test
        -- current behaviour.
        get "/v1/users/suki/keys" `shouldRespondWith` 200

-- TODO: Tests for SSH settings API:
--
-- In particular, want to show that submitting invalid keys returns some sort
-- of error and doesn't create them
