{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.User
       ( ListUsersRow(..)
       , SigninData(..)
       , SigninOK(..)
       ) where

import BasicPrelude

import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)

import Holborn.API.Types (Username)
import Holborn.Auth (AuthToken)

data ListUsersRow = ListUsersRow
    { _ListUsersRow_id :: Int
    , _ListUsersRow_username :: Username
    } deriving (Show, Generic)

instance ToJSON ListUsersRow where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_ListUsersRow_" :: String)) }

instance FromRow ListUsersRow where
    fromRow = ListUsersRow <$> field <*> field

data SigninData = SigninData
   { _SigninData_username :: Username
   , _SigninData_password :: Text
   } deriving (Show, Generic)

data SigninErrors = SigninErrors
   { _SigninErrors_username :: Maybe Text
   , _SigninErrors_password :: Maybe Text
   } deriving (Show, Generic)

data SigninOK = SigninOK
   { _SigninOK_token :: AuthToken
   } deriving (Show, Generic)

instance ToJSON SigninOK where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_SigninOK_" :: String)) }

instance ToJSON SigninErrors where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_SigninErrors_" :: String)) }

instance FromJSON SigninData where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_SigninData_" :: String)) }
