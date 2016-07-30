{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.Settings.SSHKeys
       ( ListKeysRow(..)
       , AddKeyData(..)
       ) where

import HolbornPrelude
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions)
import Data.Aeson.Types (Options(fieldLabelModifier))
import Data.Time.LocalTime (LocalTime)
import GHC.Generics (Generic)

import Holborn.JSON.SSHRepoCommunication (SSHKey)

-- XXX: Move this back to holborn-api. No reason to be here at all.

data ListKeysRow = ListKeysRow
    { _ListKeysRow_id :: Int
    , _ListKeysRow_key :: SSHKey
    , _ListKeysRow_verified :: Bool
    , _ListKeysRow_read_only :: Bool
    , _ListKeysRow_created_at :: LocalTime
    } deriving (Show, Generic)

instance ToJSON ListKeysRow where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_ListKeysRow_" :: String)) }

instance FromRow ListKeysRow where
    fromRow = ListKeysRow <$> field <*> field <*> field <*> field <*> field


data AddKeyData = AddKeyData
    { _AddKeyData_key :: Text
    } deriving (Show, Generic)

instance ToJSON AddKeyData where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_AddKeyData_" :: String)) }

instance FromJSON AddKeyData where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_AddKeyData_" :: String)) }
