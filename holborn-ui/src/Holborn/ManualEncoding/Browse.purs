module Holborn.ManualEncoding.Browse where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Lens (LensP, lens)
import Data.Generic (class Generic)
import Holborn.JSON.Generic (gDecode)
import Data.Lens (lens, LensP)


data RepoMeta = RepoMeta
    { owner :: String
    , repo :: String
    , number_commits :: Int
    , number_objects :: Int
    , size :: Int
    }


data BrowseMetaResponse = BrowseMetaResponse
  { repo_meta :: RepoMeta
  , description :: String
  , created_at :: String -- TODO time conversion
  }

description :: LensP BrowseMetaResponse String
description = lens (\(BrowseMetaResponse s) -> s.description) (\(BrowseMetaResponse s) x -> BrowseMetaResponse (s { description = x }))


derive instance genericBrowseMetaResponse :: Generic BrowseMetaResponse
derive instance genericRepoMeta :: Generic RepoMeta

instance decodeRepoMeta :: DecodeJson RepoMeta where
  decodeJson = gDecode

instance decodeBrowseMetaResponse :: DecodeJson BrowseMetaResponse where
  decodeJson = gDecode


data GitTreeEntry = GitTreeEntry { path :: String }

path :: LensP GitTreeEntry String
path = lens (\(GitTreeEntry s) -> s.path) (\(GitTreeEntry s) x -> GitTreeEntry (s { path = x }))


derive instance genericGitTreeEntry :: Generic GitTreeEntry


instance decodeGitTreeEntry :: DecodeJson GitTreeEntry where
  decodeJson = gDecode


data GitTree = GitTree
  { sha :: String
  , tree :: Array GitTreeEntry
  }

derive instance genericGitTree :: Generic GitTree

tree :: LensP GitTree (Array GitTreeEntry)
tree = lens (\(GitTree s) -> s.tree) (\(GitTree s) x -> GitTree (s { tree = x }))

instance decodeGitTree :: DecodeJson GitTree where
  decodeJson = gDecode
