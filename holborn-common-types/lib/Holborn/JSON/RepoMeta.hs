{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.RepoMeta
       ( RepoMeta(..)
       )
       where

import BasicPrelude
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)


data RepoMeta = RepoMeta
    { _RepoMeta_number_commits :: Int -- git rev-list --count master
    , _RepoMeta_number_objects :: Int -- git count-objects
    , _RepoMeta_size :: Int -- git count-objects
    -- TODO newest commit etc.
    } deriving (Show, Generic)


instance ToJSON RepoMeta where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_RepoMeta_" :: String)) }


instance FromJSON RepoMeta where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_RepoMeta_" :: String)) }
