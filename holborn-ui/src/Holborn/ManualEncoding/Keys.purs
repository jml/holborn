module Holborn.ManualEncoding.Keys where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (fromString, fromObject, toString, toObject)

import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, insert, singleton)
import Data.Either (Either(..))
import Data.Lens (LensP, lens)


-- The following three feel like they ought to be parametrized ...
data Key = Key { key :: String , title :: String }
data AddKeyData = AddKeyData { key :: String, title :: String }
data AddKeyDataError = AddKeyDataError { key :: Maybe String, title :: Maybe String }

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
