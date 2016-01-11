{-# LANGUAGE TemplateHaskell #-}
module Holborn.JSON.User
       ( ListUsersRow(..)
       ) where

import BasicPrelude
import Data.Char (toLower)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, constructorTagModifier)

data ListUsersRow = ListUsersRow
    { _listUsersRowId :: Int
    , _listUsersRowUsername :: Text
    } deriving Show
$(deriveJSON defaultOptions { fieldLabelModifier = drop (length ("_listUsersRow" :: String))
                            , constructorTagModifier = map toLower} ''ListUsersRow)


instance FromRow ListUsersRow where
    fromRow = ListUsersRow <$> field <*> field
