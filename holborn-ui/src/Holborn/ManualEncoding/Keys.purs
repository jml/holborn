module Holborn.ManualEncoding.Keys where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (toObject, toString, fromObject, fromString, Json)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, insert, singleton)
import Data.Either (Either(..))
import Data.Lens (Lens, lens, LensP)


data Key =
  Key { key :: String
      , title :: String
      }

data AddKeyData = AddKeyData { key :: String, title :: String }

title :: LensP AddKeyData String
title = lens
        (\(AddKeyData { title }) -> title)
        (\(AddKeyData { key }) t -> AddKeyData { key, title: t})

key :: LensP AddKeyData String
key = lens
        (\(AddKeyData { key }) -> key)
        (\(AddKeyData { title }) k -> AddKeyData { key: k, title: title})


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
