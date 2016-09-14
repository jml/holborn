{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.NewRepo
       ( NewRepoRequest(..)
       , Visibility(..)
       )
       where

import HolbornPrelude hiding (String)
import Data.Aeson (FromJSON(..), Value(String))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Holborn.JSON.RepoMeta (OwnerName, RepoName)
import Data.Text (unpack)

-- | Repos are either public or private
-- if private then read-access is controlled via organisation features.
data Visibility = Public | Private deriving (Show, Eq)

-- TODO ToJSON instance for Visibility

instance FromJSON Visibility where
 parseJSON (String v) = case v of
   "public" -> pure Public
   "private" -> pure Private
   _ -> fail ("invalid value: " <> (unpack v))
 parseJSON invalid = typeMismatch "Visibility" invalid


data NewRepoRequest = NewRepoRequest
    { owner :: OwnerName
    , name :: RepoName
    , description :: Maybe Text -- Optional
    , visibility :: Visibility
    } deriving (Show, Generic)


instance FromJSON NewRepoRequest
