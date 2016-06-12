module Holborn.API.Types
       ( newUsername
       , newEmail
       , newPassword
       , checkPassword
       , Username
       , Email
       , Password
       , ApiError(..)
       ) where

import HolbornPrelude hiding (empty)
import qualified Crypto.BCrypt as BCrypt
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import qualified Prelude
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.Types (typeMismatch)
import Web.HttpApiData (FromHttpApiData(..))


newtype Username = Username Text deriving (Eq, Ord, Show, ToField, FromHttpApiData)
newtype Email = Email Text deriving (Eq, Ord, Show, ToField, FromField)
newtype Password = Password ByteString deriving (ToField)


-- TODO: Validate username
newUsername :: Text -> Username
newUsername = Username


-- TODO: validate email (at least has an @ in it)
newEmail :: Text -> Maybe Email
newEmail = Just . Email

instance FromHttpApiData Email where
    parseUrlPiece piece = case newEmail piece of
      Just x -> pure x
      _ -> Left "invalid gap-auth header: must contain email"

-- | Creates a new bcrypt-encrypted password to store in the
-- database. Needs to live in IO for randomness.
newPassword :: Text -> IO Password
newPassword p = do
    pwd <- BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy (encodeUtf8 p)
    return (Password (fromMaybe (terror "bcrypt returned NULL") pwd))

checkPassword :: Password -> ByteString -> Bool
checkPassword (Password actual) toCheck =
    BCrypt.validatePassword actual toCheck

instance Prelude.Show Password where
    show _ = "*hidden-password*" :: String


instance FromField Username where
    fromField _ (Just bs) = return (Username (decodeUtf8 bs))
    fromField _ _ = terror "FromField Username should always decode correctly"

instance FromField Password where
    fromField _ (Just bs) = return (Password bs)
    fromField _ _ = terror "FromField Password should always decode correctly"

-- | Sum type of all API errors that we want to handle nicely (where
-- nicely means something other than throwing a 500 with "something
-- broke").
data ApiError =
    UserAlreadyExists Username
    | UnexpectedConstraintViolation Text
    | UserNotFound Username
    deriving Show


instance FromJSON Username where
    parseJSON (String v) = pure (Username v) -- TODO: check username is valid
    parseJSON x = typeMismatch "Username" x

instance ToJSON Username where
    toJSON (Username u) = String u

instance FromJSON Email where
    parseJSON (String v) = pure (Email v)
    parseJSON x = typeMismatch "Email" x
