{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.RepoMeta
       ( RepoMeta(..)
       , newValidRepoName
       , ValidRepoName
       )
       where

import BasicPrelude
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, Value(String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)
import qualified Data.Attoparsec.Text as AT
import Data.Char (isDigit, isAsciiUpper, isAsciiLower)
import qualified Data.Text
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Test.QuickCheck (Arbitrary(..), elements, listOf1)
import Text.Show (Show(..))
import qualified Data.Text as T

newtype ValidRepoName = ValidRepoName Text deriving (Eq, Ord)

-- The way to "escape" ValidRepoName when e.g. building a path segment
-- is via `show` so we need to show the underlying text, not
-- `ValidRepoName x`.
instance Show ValidRepoName where
    show (ValidRepoName x) = T.unpack x


instance Arbitrary ValidRepoName where
    arbitrary =
      (\a b -> ValidRepoName (fromString (a : b))) <$> (elements startAlphabet) <*> listOf1 (elements alphabet)
      where
        startAlphabet = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']
        alphabet = startAlphabet <> "-_"


validRepoNameParser :: AT.Parser Text
validRepoNameParser = do
    s <- AT.satisfy (\x -> isDigit x || isAsciiLower x || isAsciiUpper x)
    rest <- AT.takeWhile (\x -> isDigit x || isAsciiLower x || isAsciiUpper x || x == '_' || x == '-')
    AT.endOfInput
    pure (Data.Text.cons s rest)


newValidRepoName :: Text -> Maybe ValidRepoName
newValidRepoName s = case AT.parseOnly validRepoNameParser s of
    Left _ -> Nothing
    Right x -> Just (ValidRepoName x)

-- | E.g.
-- λ  decode "\"repo-name\"" :: Maybe ValidRepoName
-- Just (ValidRepoName "repo-name")
instance FromJSON ValidRepoName where
    parseJSON (String s) = case newValidRepoName s of
        Nothing -> fail ("Not a valid repository name: " ++ (Data.Text.unpack s))
        Just x -> pure x
    parseJSON x = typeMismatch "Not a valid repo type" x


instance ToJSON ValidRepoName where
    toJSON (ValidRepoName s) = String s


instance FromField ValidRepoName where
    fromField _ (Just bs) = case newValidRepoName (decodeUtf8 bs) of
        Just x -> pure x
        Nothing -> terror ("Could not parse valid repo name. " <> (decodeUtf8 bs))
    fromField _ Nothing = terror "FromField Permissions should always decode correctly"


instance ToField ValidRepoName where
    toField (ValidRepoName s) = Escape (encodeUtf8 s)


data RepoMeta = RepoMeta
    { _RepoMeta_owner :: Text
    , _RepoMeta_repo :: ValidRepoName
    , _RepoMeta_number_commits :: Int -- git rev-list --count master
    , _RepoMeta_number_objects :: Int -- git count-objects
    , _RepoMeta_size :: Int -- git count-objects
    -- TODO newest commit
    -- TODO branches, tags, ..., everything needed to render the landing page
    } deriving (Show, Generic)


instance ToJSON RepoMeta where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_RepoMeta_" :: String)) }


instance FromJSON RepoMeta where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop (length ("_RepoMeta_" :: String)) }
