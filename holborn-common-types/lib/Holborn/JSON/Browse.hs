{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Holborn.JSON.Browse
       ( BrowseCommitResponse(..)
       , BrowseMetaResponse(..)
       , BrowseTreeResponse(..)
       , AuthorInfo(..)
       , TreeObject(..)
       ) where

import HolbornPrelude
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Holborn.JSON.RepoMeta (RepoMeta)

data BrowseMetaResponse = BrowseMetaResponse
    { repo_meta :: RepoMeta
    , description :: Text
    , created_at :: UTCTime
    -- TODO so many missing fields
    } deriving (Show, Generic)


instance ToJSON BrowseMetaResponse

data AuthorInfo = AuthorInfo
    { date :: UTCTime
    , name :: Text
    , email :: Text
    } deriving (Show, Generic)

instance ToJSON AuthorInfo

data BrowseCommitResponse = BrowseCommitResponse
    { sha :: Text
    , url :: Text
    , author :: AuthorInfo
    , committer :: AuthorInfo
    -- TODO parent like in https://developer.github.com/v3/git/commits/ ?
    } deriving (Show, Generic)

instance ToJSON BrowseCommitResponse

data TreeObject = TreeObject
    { path :: Text
    , mode :: Text
    , objectType :: Text -- TODO encode a as sum type?
    , size :: Int
    , sha :: Text -- TODO special sha type?
    , url :: Text
    } deriving (Show, Generic)

instance ToJSON TreeObject

data BrowseTreeResponse = BrowseTreeResponse
    { sha :: Text
    , truncated :: Text
    , url :: Text
    , tree :: [TreeObject]
    } deriving (Show, Generic)

instance ToJSON BrowseTreeResponse
