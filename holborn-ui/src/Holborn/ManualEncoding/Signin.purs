module Holborn.ManualEncoding.Signin where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (fromString, fromObject, toString, toObject)
import Data.Generic (class Generic)
import Data.Lens (LensP, lens)
import Data.Maybe (Maybe(..))
import Holborn.JSON.Generic (gDecode, gEncode)


data SigninData = SigninData { username :: String, password :: String }
data SigninDataErrors = SigninDataErrors { username :: Maybe String, password :: Maybe String }
data SigninOK = SigninOK { token :: String }


derive instance genericSigninData :: Generic SigninData
derive instance genericSigninDataErrors :: Generic SigninDataErrors
derive instance genericSigninOK :: Generic SigninOK


username :: LensP SigninData String
username = lens
        (\(SigninData { username }) -> username)
        (\(SigninData { password }) x -> SigninData { username: x, password})

password :: LensP SigninData String
password = lens
        (\(SigninData { password }) -> password)
        (\(SigninData { username }) x -> SigninData { username, password: x})


instance encodeSigninData :: EncodeJson SigninData where
  encodeJson = gEncode

instance decodeSigninDataErrors :: DecodeJson SigninDataErrors where
  decodeJson = gDecode

instance decodeSigninOK :: DecodeJson SigninOK where
  decodeJson = gDecode
