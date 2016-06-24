{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.RepoMeta
       ( RepoMeta(..)
       , newRepoName
       , RepoName
       , repoNameParser
       , RepoId
       )
       where

import HolbornPrelude

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, Value(String), withText)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)
import qualified Data.Attoparsec.Text as AT
import Data.Char (isDigit, isAsciiUpper, isAsciiLower)
import qualified Data.Text
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Test.QuickCheck (Arbitrary(..), elements, listOf1)
import Web.HttpApiData (ToHttpApiData(..))


newtype RepoName = RepoName Text deriving (Eq, Ord, Show)


type RepoId = Int

-- The way to "escape" RepoName when e.g. building a path segment
-- is via `show` so we need to show the underlying text, not
-- `RepoName x`.
instance ToHttpApiData RepoName where
    toUrlPiece (RepoName x) = x


instance Arbitrary RepoName where
    arbitrary =
      (RepoName . fromString) <$> validString
      where
        validString = (:) <$> elements startAlphabet <*> listOf1 (elements alphabet)
        startAlphabet = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']
        alphabet = startAlphabet <> "-_"


repoNameParser :: AT.Parser RepoName
repoNameParser = do
    s <- AT.satisfy (\x -> isDigit x || isAsciiLower x || isAsciiUpper x)
    rest <- AT.takeWhile (\x -> isDigit x || isAsciiLower x || isAsciiUpper x || x == '_' || x == '-')
    pure $ RepoName (Data.Text.cons s rest)


newRepoName :: Alternative m => Text -> m RepoName
newRepoName s = hush (AT.parseOnly repoNameParser s)


-- | E.g.
-- λ  decode "\"repo-name\"" :: Maybe RepoName
-- Just (RepoName "repo-name")
instance FromJSON RepoName where
    parseJSON = withText "RepoName must be text" newRepoName

instance ToJSON RepoName where
    toJSON (RepoName s) = String s

instance FromField RepoName where
    fromField _ (Just bs) = case newRepoName (decodeUtf8 bs) of
        Just x -> pure x
        Nothing -> terror ("Could not parse repo name. " <> decodeUtf8 bs)
    fromField _ Nothing = terror "FromField Permissions should always decode correctly"


instance ToField RepoName where
    toField (RepoName s) = Escape (encodeUtf8 s)


-- | This is what we're sending to users who query repository meta
-- data. GH return this:
-- https://developer.github.com/v3/repos/#list-organization-repositories
--
-- TODO: rename to ProjectMeta
-- TODO: put in the fields we think are needed to write tools
data RepoMeta = RepoMeta
    { _RepoMeta_id :: RepoId
    , _RepoMeta_number_commits :: Int -- git rev-list --count master
    , _RepoMeta_number_objects :: Int -- git count-objects
    , _RepoMeta_size :: Int -- git count-objects
    , _RepoMeta_owner :: Text -- TODO make owner a validated newtype (ValidOwner)
    -- TODO newest commit
    -- TODO branches, tags, ..., everything needed to render the landing page
    } deriving (Show, Generic)


instance ToJSON RepoMeta where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_RepoMeta_" :: String)) }


instance FromJSON RepoMeta where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_RepoMeta_" :: String)) }
