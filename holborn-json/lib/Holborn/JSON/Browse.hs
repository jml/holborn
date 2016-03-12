{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.Browse
       ( BrowseRequest(..)
       , BrowseCommitResponse(..)
       , BrowseTreeResponse(..)
       , AuthorInfo(..)
       , TreeObject(..)
       ) where

import BasicPrelude
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier, constructorTagModifier)
import GHC.Generics (Generic)
import Data.Time.LocalTime (LocalTime)


data BrowseRequest = BrowseRequest
    { _BrowseRequest_repo :: Text -- Repo is an org or user / reponame e.g. "jml/holborn"
    , _BrowseRequest_path :: Text -- e.g. ""
    } deriving (Show, Generic)


instance FromJSON BrowseRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_BrowseRequest_" :: String)) }


data AuthorInfo = AuthorInfo
    { _AuthorInfo_date :: LocalTime
    , _AuthorInfo_name :: Text
    , _AuthorInfo_email :: Text
    } deriving (Show, Generic)

instance ToJSON AuthorInfo where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_AuthorInfo_" :: String)) }


data BrowseCommitResponse = BrowseCommitResponse
    { _BrowseCommitResponse_sha :: Text
    , _BrowseCommitResponse_url :: Text
    , _BrowseCommitResponse_author :: AuthorInfo
    , _BrowseCommitResponse_committer :: AuthorInfo
    -- TODO parent like in https://developer.github.com/v3/git/commits/ ?
    } deriving (Show, Generic)

instance ToJSON BrowseCommitResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_BrowseCommitResponse_" :: String)) }


data TreeObject = TreeObject
    { _TreeObject_path :: Text
    , _TreeObject_mode :: Text
    , _TreeObject_type :: Text -- TODO encode a as sum type?
    , _TreeObject_size :: Int
    , _TreeObject_sha :: Text -- TODO special sha type?
    , _TreeObject_url :: Text
    } deriving (Show, Generic)

instance ToJSON TreeObject where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_TreeObject_" :: String)) }


data BrowseTreeResponse = BrowseTreeResponse
    { _BrowseTreeResponse_sha :: Text
    , _BrowseTreeResponse_truncated :: Text
    , _BrowseTreeResponse_url :: Text
    , _BrowseTreeResponse_tree :: [TreeObject]
    } deriving (Show, Generic)

instance ToJSON BrowseTreeResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_BrowseTreeResponse_" :: String)) }
