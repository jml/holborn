{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.Browse
       ( BrowseCommitResponse(..)
       , BrowseMetaResponse(..)
       , BrowseTreeResponse(..)
       , AuthorInfo(..)
       , TreeObject(..)
       ) where

import BasicPrelude
import Data.Aeson (ToJSON(..), genericToJSON)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)
import Data.Time.LocalTime (LocalTime)
import Holborn.JSON.RepoMeta (RepoMeta)

data BrowseMetaResponse = BrowseMetaResponse
    { _BrowseMetaResponse_repo_meta :: RepoMeta
    , _BrowseMetaResponse_description :: Text
    , _BrowseMetaResponse_created_at :: LocalTime
    -- TODO so many missing fields
    } deriving (Show, Generic)


instance ToJSON BrowseMetaResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_BrowseMetaResponse_" :: String)) }


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
