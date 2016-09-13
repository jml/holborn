module Holborn.ManualEncoding.CreateRepository where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Core (toString, fromString, fromObject, Json(..), JObject)
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Argonaut.Decode.Combinators ((.??))
import Data.StrMap as StrMap

import Data.Maybe (Maybe(..))
import Data.Lens (LensP, lens)
import Data.Lens.Prism (PrismP, prism)
import Data.Generic (class Generic)
import Data.Either (Either(..))
import Holborn.JSON.Generic (gDecode, gEncode)
import Debug.Trace

data Visibility = Private | Public

instance eqVisibility :: Eq Visibility where
  eq Public Public = true
  eq Private Private = true
  eq _ _ = false


instance encodeVisibility :: EncodeJson Visibility where
  encodeJson Private = fromString "private"
  encodeJson Public = fromString "public"

instance decodeVisibility :: DecodeJson Visibility where
  decodeJson s = case toString s of
    Just "private" -> Right Private
    Just "public" -> Right Public
    _ -> Left ("Could not decode: " <> show s)


-- The following three feel like they ought to be parametrized ...
data CreateRepositoryData =
  CreateRepositoryData { name :: Maybe String
                       , description :: Maybe String
                       , visibility :: Maybe Visibility
                       , owner :: Maybe String
                       }

empty = CreateRepositoryData { name: Nothing, description: Just "",  visibility: Just Public, owner: Nothing }

derive instance genericCreateRepositoryData :: Generic CreateRepositoryData
derive instance genericVisibility :: Generic Visibility

name :: LensP CreateRepositoryData (Maybe String)
name = lens
        (\(CreateRepositoryData { name }) -> name)
        (\(CreateRepositoryData o) x -> CreateRepositoryData (o { name = x }))

description :: LensP CreateRepositoryData (Maybe String)
description = lens
        (\(CreateRepositoryData { description }) -> description)
        (\(CreateRepositoryData o) x -> CreateRepositoryData (o { description = x }))

visibility :: LensP CreateRepositoryData (Maybe Visibility)
visibility = lens
        (\(CreateRepositoryData { visibility }) -> visibility)
        (\(CreateRepositoryData o) x -> CreateRepositoryData (o { visibility = x }))

owner :: LensP CreateRepositoryData (Maybe String)
owner = lens
        (\(CreateRepositoryData { owner }) -> owner)
        (\(CreateRepositoryData o) x -> CreateRepositoryData (o { owner = x }))


_Public :: PrismP (Maybe Visibility) Boolean
_Public = prism from to
  where
    from x = case x of
      true -> traceAnyM "truevalue" *> Just Public
      x -> traceAnyM x *> Just Private
    to x = case x of
      Just Public -> Right true
      x -> Left x


-- Custom decoder because purescript generic encoder doesn't encode
-- sum types correctly.
instance decodeCreateRepositoryData :: DecodeJson CreateRepositoryData where
  decodeJson json' = case decodeJson json' of
    Left err -> Left err
    Right json -> do
      name <- json .?? "name"
      description <- json .?? "description"
      owner <- json .?? "owner"
      visibility <- json .?? "visibility"
      pure (CreateRepositoryData { name, description, owner, visibility })


instance encodeCreateRepositoryData :: EncodeJson CreateRepositoryData where
  encodeJson (CreateRepositoryData x) =
    "owner" := x.owner
    ~> "description" := x.description
    ~> "name" := x.name
    ~> "visibility" := (spy x.visibility)
    ~> unit -- TODO not sure why I need to finish of with unit here?
