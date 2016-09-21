{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Holborn.JSON.Settings.SSHKeys
       ( ListKeysRow(..)
       , AddKeyData(..)
       ) where

import HolbornPrelude
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Holborn.JSON.SSHRepoCommunication (SSHKey)

-- XXX: Move this back to holborn-api. No reason to be here at all.

data ListKeysRow = ListKeysRow
    { id :: Int
    , key :: SSHKey
    , verified :: Bool
    , readonly :: Bool
    , created_at :: UTCTime
    } deriving (Show, Generic)

instance ToJSON ListKeysRow

data AddKeyData = AddKeyData
    { key :: Text
    } deriving (Show, Generic)

instance ToJSON AddKeyData
instance FromJSON AddKeyData
