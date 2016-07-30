-- | Tests for Holborn.API.Settings.SSHKeys

module Settings.SSHKeys (spec) where

import HolbornPrelude

import Test.Tasty.Hspec (SpecWith, describe, it)
import Test.Hspec.Wai.Internal (withApplication)

import Holborn.API.Config (Config)

import Fixtures (makeTestApp)

spec :: SpecWith Config
spec = do
  describe "/v1/users/<username>/keys" $
    it "404s for non-existent users" $ \config -> do
      withApplication (makeTestApp config) $ do
        pure ()

-- TODO: Tests for SSH settings API:
--
-- In particular, want to show that submitting invalid keys returns some sort
-- of error and doesn't create them
