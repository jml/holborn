module Holborn.API.Types
       ( newUsername
       , newEmail
       , newPassword
       , checkPassword
       , newDexMail
       , Username
       , Email
       , Password
       , DexMail
       ) where

import HolbornPrelude
import qualified Crypto.BCrypt as BCrypt
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import qualified Prelude
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.Types (typeMismatch)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))


newtype Username = Username Text deriving (Eq, Ord, Show, ToField, FromHttpApiData, ToHttpApiData, IsString)
newtype DexMail = DexMail Text deriving (Eq, Ord, Show, ToField, FromField, FromHttpApiData, ToHttpApiData, IsString)

newtype Email = Email Text deriving (Eq, Ord, Show, ToField, FromField, ToHttpApiData)
newtype Password = Password ByteString deriving (ToField)


-- TODO: Validate username
newUsername :: Text -> Username
newUsername = Username


newDexMail :: Text -> DexMail
newDexMail = DexMail


-- TODO: validate email (at least has an @ in it)
newEmail :: Text -> Maybe Email
newEmail = Just . Email

instance FromHttpApiData Email where
    parseUrlPiece piece = case newEmail piece of
      Just x -> pure x
      -- TODO this looks out of date
      _ -> Left "invalid x-dex-email header: must contain email"


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


instance FromJSON Username where
    parseJSON (String v) = pure (Username v) -- TODO: check username is valid
    parseJSON x = typeMismatch "Username" x

instance ToJSON Username where
    toJSON (Username u) = String u

instance FromJSON Email where
    parseJSON (String v) = pure (Email v)
    parseJSON x = typeMismatch "Email" x
