module Holborn.ManualEncoding.Keys where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (fromString, fromObject, toString, toObject)

import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, insert, singleton)
import Data.Either (Either(..))
import Data.Lens (LensP, lens)
import Data.Generic (class Generic)

import Holborn.JSON.Generic (gDecode)

-- The following three feel like they ought to be parametrized ...
data Key = Key { key :: String , title :: String }
data AddKeyData = AddKeyData { key :: String, title :: String }
data AddKeyDataError = AddKeyDataError { key :: Maybe String, title :: Maybe String }

derive instance genericKey :: Generic Key

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


instance encodeAddKeyData :: EncodeJson AddKeyData where
  encodeJson (AddKeyData ak) = fromObject (insert "title" (fromString ak.title) (singleton "key" (fromString ak.key)))
