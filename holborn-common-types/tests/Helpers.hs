module Helpers
  ( jsonIdentity
  , httpApiDataIdentity
  ) where

import HolbornPrelude

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Test.Tasty.QuickCheck
  ( (===)
  , Property
  )
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

-- | Objects can be encoded to JSON and then decoded from JSON, resulting in
-- the same object.
jsonIdentity :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
jsonIdentity x = Just x === decode (encode x)

-- | Objects can be encoded for URLs and then decoded, resulting in the same
-- object.
httpApiDataIdentity :: (Eq a, Show a, FromHttpApiData a, ToHttpApiData a) => a -> Property
httpApiDataIdentity x = Right x === parseUrlPiece (toUrlPiece x)
