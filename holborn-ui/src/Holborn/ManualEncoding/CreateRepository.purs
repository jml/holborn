module Holborn.ManualEncoding.CreateRepository where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Core (toString, fromString)
import Data.Argonaut.Encode (class EncodeJson)

import Data.Maybe (Maybe(..))
import Data.Lens (LensP, lens)
import Data.Generic (class Generic)
import Data.Either (Either(..))
import Holborn.JSON.Generic (gDecode, gEncode)


data Visibility = Private | Public

instance encodeVisibility :: EncodeJson Visibility where
  encodeJson Private = fromString "private"
  encodeJson Public = fromString "public"

instance decodeVisibility :: DecodeJson Visibility where
  decodeJson s = case toString s of
    Just "private" -> Right Private
    Just "public" -> Right Public
    _ -> Left ("Could not decode: " <> show s)


-- The following three feel like they ought to be parametrized ...
data CreateRepositoryData = CreateRepositoryData { repoName :: Maybe String, description :: Maybe String, visibility :: Maybe Visibility }

empty = CreateRepositoryData { repoName: Nothing, description: Nothing,  visibility: Nothing}

derive instance genericCreateRepositoryData :: Generic CreateRepositoryData
derive instance genericVisibility :: Generic Visibility

repoName :: LensP CreateRepositoryData (Maybe String)
repoName = lens
        (\(CreateRepositoryData { repoName }) -> repoName)
        (\(CreateRepositoryData o) x -> CreateRepositoryData (o { repoName = x }))

description :: LensP CreateRepositoryData (Maybe String)
description = lens
        (\(CreateRepositoryData { description }) -> description)
        (\(CreateRepositoryData o) x -> CreateRepositoryData (o { description = x }))

visibility :: LensP CreateRepositoryData (Maybe Visibility)
visibility = lens
        (\(CreateRepositoryData { visibility }) -> visibility)
        (\(CreateRepositoryData o) x -> CreateRepositoryData (o { visibility = x }))


instance decodeCreateRepositoryData :: DecodeJson CreateRepositoryData where
  decodeJson = gDecode

instance encodeCreateRepositoryData :: EncodeJson CreateRepositoryData where
  encodeJson = gEncode
