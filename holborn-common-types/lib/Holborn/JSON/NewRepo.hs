{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.NewRepo
       ( NewRepoRequest(..)
       )
       where

import HolbornPrelude
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)
import Holborn.JSON.RepoMeta (RepoName)

data NewRepoRequest = NewRepoRequest
    { _NewRepoRequest_owner :: Text
    , _NewRepoRequest_name :: RepoName
    , _NewRepoRequest_description :: Text
    , _NewRepoRequest_private :: Bool
    , _NewRepoRequest_initialize :: Bool
    } deriving (Show, Generic)


instance FromJSON NewRepoRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_NewRepoRequest_" :: String)) }
