{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.Response
       ( PaginatedResponse(..)
       , Result(..)
       ) where

import BasicPrelude
import GHC.Generics (Generic)
import Data.Char (toLower)

import Data.Aeson (ToJSON(..), genericToJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier, constructorTagModifier)

-- TODO: We will move the pagination data into http headers to make
-- trivial API requests easier to decode.
data PaginatedResponse a = PaginatedResponse
    { _PaginatedResponse_values :: a
    , _PaginatedResponse_next :: Text -- token to identify next page
    } deriving (Show, Generic)

instance ToJSON a => ToJSON (PaginatedResponse a) where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_PaginatedResponse_" :: String))
    , constructorTagModifier = map toLower}


data Result ok err = OK { r :: ok } | Error { error :: err } deriving (Show, Generic)

instance (ToJSON ok, ToJSON err) =>  ToJSON (Result ok err)
