{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Holborn.CommonTypes.Repo
  ( RepoName
  , newRepoName
  , repoNameParser
  , OwnerName
  , newOwnerName
  , ownerNameParser
  , RepoId
  ) where

import HolbornPrelude

import Data.Aeson (ToJSON(..), FromJSON(..), Value(String), withText)
import qualified Data.Attoparsec.Text as AT
import Data.Char (isDigit, isAsciiUpper, isAsciiLower)
import qualified Data.Text
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Test.QuickCheck (Arbitrary(..), elements, listOf1)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))


type RepoId = Int

data Names = Repo | Owner
data Name (name :: Names) = Name Text deriving (Eq, Ord, Show)

type RepoName = Name 'Repo
type OwnerName = Name 'Owner

instance Arbitrary (Name a) where
    arbitrary =
      (Name . fromString) <$> validString
      where
        validString = (:) <$> elements startAlphabet <*> listOf1 (elements alphabet)
        startAlphabet = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']
        alphabet = startAlphabet <> "-_"

nameParser :: AT.Parser (Name a)
nameParser = do
    s <- AT.satisfy (\x -> isDigit x || isAsciiLower x || isAsciiUpper x)
    rest <- AT.takeWhile (\x -> isDigit x || isAsciiLower x || isAsciiUpper x || x == '_' || x == '-')
    pure $ Name (Data.Text.cons s rest)

newName :: Alternative m => Text -> m (Name a)
newName s = hush (AT.parseOnly nameParser s)

-- | How we turn names that appear in URLs & query parameters into Name
-- (OwnerName, RepoName) objects.
instance FromHttpApiData (Name a) where
    parseUrlPiece s = fmapL fromString (AT.parseOnly nameParser s)

-- | The way to "escape" names when building path segments, headers or query
-- parameters.
instance ToHttpApiData (Name a) where
    toUrlPiece (Name x) = x

-- | E.g.
-- λ  decode "\"repo-name\"" :: Maybe RepoName
-- Just (RepoName "repo-name")
instance FromJSON (Name a) where
    parseJSON = withText "RepoName must be text" newName

instance ToJSON (Name a) where
    toJSON (Name s) = String s

instance FromField (Name a) where
    -- TODO: Remove these partial functions.
    fromField _ (Just bs) = case newName (decodeUtf8 bs) of
        Just x -> pure x
        Nothing -> terror ("Could not parse repo name. " <> decodeUtf8 bs)
    fromField _ Nothing = terror "FromField Permissions should always decode correctly"


instance ToField (Name a) where
    toField (Name s) = Escape (encodeUtf8 s)


newRepoName :: Alternative m => Text -> m RepoName
newRepoName = newName

repoNameParser :: AT.Parser RepoName
repoNameParser = nameParser

newOwnerName :: Alternative m => Text -> m OwnerName
newOwnerName = newName

ownerNameParser :: AT.Parser OwnerName
ownerNameParser = nameParser
