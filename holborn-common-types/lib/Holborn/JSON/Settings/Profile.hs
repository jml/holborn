{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.Settings.Profile
       ( ProfileData(..)
       ) where

import HolbornPrelude
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.LocalTime (LocalTime)
import GHC.Generics (Generic)

-- TODO - the response profile data should be much more detailed
-- (number of repos, gravatar, followers, following, gists, company,
-- ...)
data ProfileData = ProfileData
    { id :: Int
    , username :: Text
    , about :: Text
    , date_joined :: LocalTime
    } deriving (Show, Generic)


instance ToJSON ProfileData
instance FromJSON ProfileData
