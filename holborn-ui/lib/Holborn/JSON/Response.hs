{-# LANGUAGE TemplateHaskell #-}
module Holborn.JSON.Response
       ( PaginatedResponse(..)
       , Result(..)
       ) where

import BasicPrelude
import Data.Char (toLower)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, constructorTagModifier)


data PaginatedResponse a = PaginatedResponse
    { _paginatedResponseValues :: a
    , _paginatedResponseNext :: Text -- token to identify next page
    } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop (length ("_paginatedResponse" :: String))
                            , constructorTagModifier = map toLower} ''PaginatedResponse)

data Result ok err = OK { r :: ok } | Error { error :: err } deriving (Show)
$(deriveJSON defaultOptions ''Result)
