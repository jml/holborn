{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.CreateAccount
       ( CreateAccountRequest(..)
       )
       where

import HolbornPrelude
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Holborn.JSON.RepoMeta (OwnerName)


data CreateAccountRequest = CreateAccountRequest
    { username :: OwnerName
    } deriving (Show, Generic)


instance FromJSON CreateAccountRequest
