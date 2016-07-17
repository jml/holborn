module Components.Response where

import Prelude

import Data.Argonaut.Core (Json(), fromObject, toObject, toString, fromString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Maybe
import Data.StrMap as SM
import Data.Either as Either

data Response r err = OK r | Error err


tryDecode :: forall a. String -> (Json -> Maybe a) -> Json -> Either.Either String a
tryDecode label f v = case f v of
  Nothing -> Either.Left ("Label not found: " <> label)
  Just x -> Either.Right x


tryLookup :: String -> SM.StrMap Json -> Either.Either String Json
tryLookup label sm = case SM.lookup label sm of
  Nothing -> Either.Left ("Key not found: " <> label)
  Just x -> Either.Right x


instance decodeResponse :: (DecodeJson r, DecodeJson err) => DecodeJson (Response r err) where
  decodeJson json = do
    o <- tryDecode "object" toObject json
    tagJson <- tryLookup "tag" o
    tag <- tryDecode "tag" toString tagJson
    case tag of
      "Error" -> do
        rJson <- tryLookup "error" o
        map Error (decodeJson rJson)
      "OK" -> do
        rJson <- tryLookup "r" o
        map OK (decodeJson rJson)
      _ -> map Error (decodeJson (fromString "unexpected decode"))
