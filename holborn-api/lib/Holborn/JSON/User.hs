{-# LANGUAGE TemplateHaskell #-}
module Holborn.JSON.User
       ( ListUsersRow(..)
       ) where

import BasicPrelude
import Data.Char (toLower)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, constructorTagModifier)

data ListUsersRow = ListUsersRow
    { _ListUsersRow_id :: Int
    , _ListUsersRow_username :: Text
    } deriving Show
$(deriveJSON defaultOptions { fieldLabelModifier = drop (length ("_ListUsersRow_" :: String))
                            , constructorTagModifier = map toLower} ''ListUsersRow)


instance FromRow ListUsersRow where
    fromRow = ListUsersRow <$> field <*> field
