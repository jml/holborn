module Holborn.ManualEncoding.Keys where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (toObject, toString, fromObject, fromString, Json)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, insert, singleton)
import Data.Either (Either(..))


data Key =
  Key { key :: String
      , title :: String
      }

data AddKeyData = AddKeyData { key :: String, title :: String }

instance decodeKey :: DecodeJson Key where
  decodeJson json =
    case dec of
      Nothing -> Left "no decode"
      Just x -> Right x
    where
      dec = do
        s <- toObject json
        key <- (lookup "key" s) >>= toString
        title <- (lookup "title" s) >>= toString
        return (Key { key, title })

instance encodeAddKeyData :: EncodeJson AddKeyData where
  encodeJson (AddKeyData ak) = fromObject (insert "title" (fromString ak.title) (singleton "key" (fromString ak.key)))
