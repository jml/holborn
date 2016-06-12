{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.Settings.Profile
       ( ProfileData(..)
       ) where

import HolbornPrelude
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions)
import Data.Aeson.Types (Options(fieldLabelModifier))
import Data.Time.LocalTime (LocalTime)
import GHC.Generics (Generic)

-- TODO - the response profile data should be much more detailed
-- (number of repos, gravatar, followers, following, gists, company,
-- ...)
data ProfileData = ProfileData
    { _ProfileData_id :: Int
    , _ProfileData_username :: Text
    , _ProfileData_about :: Text
    , _ProfileData_date_joined :: LocalTime
    } deriving (Show, Generic)


instance ToJSON ProfileData where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_ProfileData_" :: String)) }

instance FromJSON ProfileData where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_ProfileData_" :: String)) }
