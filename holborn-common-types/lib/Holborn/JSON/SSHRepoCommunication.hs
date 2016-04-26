{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.SSHRepoCommunication
       ( RepoCall(..)
       ) where

import BasicPrelude (String, Show, Text, drop, length)

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)


data RepoCall = RepoCall { _command :: Text, _org :: Text, _repo :: Text } deriving (Show, Generic)


instance FromJSON RepoCall where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop (length ("_" :: String))}


instance ToJSON RepoCall where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop (length ("_" :: String))}
