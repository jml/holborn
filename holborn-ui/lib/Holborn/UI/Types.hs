{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Holborn.UI.Types
       ( newUsername
       , newEmail
       , newPassword
       , Username
       , Email
       , Password
       ) where

import BasicPrelude
import qualified Crypto.BCrypt as BCrypt
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Prelude

newtype Username = Username Text deriving (Eq, Ord, Show, ToField)
newtype Email = Email Text deriving (Eq, Ord, Show, ToField)
newtype Password = Password Text deriving (ToField)


newUsername :: Text -> Username
newUsername = Username

newEmail :: Text -> Email
newEmail = Email

-- | Creates a new bcrypt-encrypted password to store in the
-- database. Needs to live in IO for randomness.
newPassword :: Text -> IO Password
newPassword p = do
    pwd <- (BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy (encodeUtf8 p))
    return (Password (decodeUtf8 (fromMaybe (terror "bcrypt returned NULL") pwd)))


instance Prelude.Show Password where
    show _ = "*hidden-password*" :: String
