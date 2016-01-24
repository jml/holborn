{-# LANGUAGE TemplateHaskell #-}
module Holborn.JSON.Keys
       ( ListKeysRow(..)
       , AddKeyData(..)
       ) where

import BasicPrelude
import Data.Char (toLower)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, constructorTagModifier)

data ListKeysRow = ListKeysRow
    { _id :: Int
    , _key :: Text
    , _title :: Text
    , _verified :: Bool
    , _read_only :: Bool
    , _created_at :: Int
    } deriving Show

$(deriveJSON defaultOptions { fieldLabelModifier = drop (length ("_" :: String))
                            , constructorTagModifier = map toLower} ''ListKeysRow)

data AddKeyData = AddKeyData
    { _add_key :: Text
    , _add_title :: Text
    } deriving Show

$(deriveJSON defaultOptions { fieldLabelModifier = drop (length ("_add_" :: String))
                            , constructorTagModifier = map toLower} ''AddKeyData)
