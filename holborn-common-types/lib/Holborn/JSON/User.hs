{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.User
       ( ListUsersRow(..)
       ) where

import BasicPrelude

import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Data.Aeson (ToJSON(..), genericToJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)

import Holborn.API.Types (Username)

data ListUsersRow = ListUsersRow
    { _ListUsersRow_id :: Int
    , _ListUsersRow_username :: Username
    } deriving (Show, Generic)

instance ToJSON ListUsersRow where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_ListUsersRow_" :: String)) }

instance FromRow ListUsersRow where
    fromRow = ListUsersRow <$> field <*> field
