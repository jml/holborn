{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.Keys
       ( ListKeysRow(..)
       , AddKeyData(..)
       ) where

import BasicPrelude
import Data.Char (toLower)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.Time (UTCTimestamp)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, constructorTagModifier)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Time.LocalTime (LocalTime)
import GHC.Generics (Generic)

import Holborn.API.Types (SSHKey, parseSSHKey)

data ListKeysRow = ListKeysRow
    { _ListKeysRow_id :: Int
    , _ListKeysRow_key :: SSHKey
    , _ListKeysRow_title :: Text
    , _ListKeysRow_verified :: Bool
    , _ListKeysRow_read_only :: Bool
    , _ListKeysRow_created_at :: LocalTime
    } deriving (Show, Generic)


instance ToJSON ListKeysRow where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_ListKeysRow_" :: String))
    , constructorTagModifier = map toLower
    }


data AddKeyData = AddKeyData
    { _AddKeyData_key :: Text
    , _AddKeyData_title :: Text
    } deriving (Show, Generic)


instance FromJSON AddKeyData where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_AddKeyData_" :: String))
    , constructorTagModifier = map toLower
    }


instance FromRow ListKeysRow where
    fromRow = ListKeysRow <$> field <*> field <*> field <*> field <*> field <*> field
