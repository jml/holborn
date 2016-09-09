module Holborn.ManualEncoding.CreateAccount where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)

import Data.Maybe (Maybe(..))
import Data.Lens (LensP, lens)
import Data.Generic (class Generic)

import Holborn.JSON.Generic (gDecode, gEncode)


-- The following three feel like they ought to be parametrized ...
data CreateAccountData = CreateAccountData { username :: Maybe String }

derive instance genericCreateAccountData :: Generic CreateAccountData

username :: LensP CreateAccountData (Maybe String)
username = lens
        (\(CreateAccountData { username }) -> username)
        (\_ x -> CreateAccountData { username: x })


instance decodeCreateAccountData :: DecodeJson CreateAccountData where
  decodeJson = gDecode

instance encodeCreateAccountData :: EncodeJson CreateAccountData where
  encodeJson = gEncode
