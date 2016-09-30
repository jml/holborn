{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Holborn.Repo.JSON.RepoMeta
  ( RepoMeta(..)
  ) where

import HolbornPrelude

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- | This is what we're sending to users who query repository meta
-- data. GH return this:
-- https://developer.github.com/v3/repos/#list-organization-repositories
--
-- TODO: rename to ProjectMeta
-- TODO: put in the fields we think are needed to write tools
data RepoMeta = RepoMeta
    { number_commits :: Int -- git rev-list --count master
    , number_objects :: Int -- git count-objects
    , size :: Int -- git count-objects
    } deriving (Show, Generic)


instance ToJSON RepoMeta
instance FromJSON RepoMeta
