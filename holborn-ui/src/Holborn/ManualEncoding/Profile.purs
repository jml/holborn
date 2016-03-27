module Holborn.ManualEncoding.Profile where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Lens (LensP, lens)
import Data.Generic (class Generic)
import Holborn.JSON.Generic (gDecode)
import Data.Lens (lens, LensP)


data Profile = Profile
    { id :: Int
    , username :: String
    , about :: String
    , date_joined :: String -- TODO localtime
    }


derive instance genericProfile :: Generic Profile


instance decodeProfile :: DecodeJson Profile where
  decodeJson = gDecode
