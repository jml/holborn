module Holborn.ManualEncoding.SSHKeys where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Lens (LensP, lens)
import Data.Generic (class Generic)

import Holborn.JSON.Generic (gDecode, gEncode)

type DateTime = String -- TODO implement parser

-- The following three feel like they ought to be parametrized ...
data Key = Key { id :: Int, key :: { key :: String, comment :: String, type_ :: String }
               , verified :: Boolean, readonly :: Boolean, created_at :: DateTime }
data AddKeyData = AddKeyData { key :: String, title :: String }
data AddKeyDataError = AddKeyDataError { global :: Maybe String, key :: Maybe String, title :: Maybe String }


derive instance genericKey :: Generic Key
derive instance genericAddKeyDataError :: Generic AddKeyDataError
derive instance genericAddKeyData :: Generic AddKeyData


title :: LensP AddKeyData String
title = lens
        (\(AddKeyData { title }) -> title)
        (\(AddKeyData { key }) t -> AddKeyData { key, title: t})

key :: LensP AddKeyData String
key = lens
        (\(AddKeyData { key }) -> key)
        (\(AddKeyData { title }) k -> AddKeyData { key: k, title: title})


instance decodeKey :: DecodeJson Key where
  decodeJson = gDecode

instance decodeAddKeyDataError :: DecodeJson AddKeyDataError where
  decodeJson = gDecode

instance encodeAddKeyData :: EncodeJson AddKeyData where
  encodeJson = gEncode
