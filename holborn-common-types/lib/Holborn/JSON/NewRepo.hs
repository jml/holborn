{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.NewRepo
       ( NewRepoRequest(..)
       )
       where

import HolbornPrelude
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Holborn.JSON.RepoMeta (OwnerName, RepoName)

data NewRepoRequest = NewRepoRequest
    { owner :: OwnerName
    , name :: RepoName
    , description :: Text
    , private :: Bool
    , initialize :: Bool
    } deriving (Show, Generic)


instance FromJSON NewRepoRequest