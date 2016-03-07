{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Holborn.API.Types
       ( newUsername
       , newEmail
       , newPassword
       , checkPassword
       , Username
       , Email
       , Password
       , ApiError(..)
       , AppConf(..)
       , SSHKey
       , KeyType(..)
       , parseSSHKey
       ) where

import BasicPrelude
import qualified Crypto.BCrypt as BCrypt
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(Escape))
import Database.PostgreSQL.Simple (Connection)
import qualified Prelude
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=))
import Data.Aeson.Types (typeMismatch)
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(ConversionFailed))
import Network.HTTP.Client (Manager)
import System.Process (runInteractiveCommand)
import System.IO.Unsafe (unsafePerformIO) -- Temporary hack until we have a pure fingerprinter
import Data.ByteString as BS
import Data.ByteString.Char8 as BS8
import System.IO (hClose)

newtype Username = Username Text deriving (Eq, Ord, Show, ToField)
newtype Email = Email Text deriving (Eq, Ord, Show, ToField)
newtype Password = Password ByteString deriving (ToField)


data AppConf = AppConf
  { conn :: Connection
  , jwtSecret :: Text
  , httpManager :: Manager
  }


newUsername :: Text -> Username
newUsername = Username

newEmail :: Text -> Email
newEmail = Email

-- | Creates a new bcrypt-encrypted password to store in the
-- database. Needs to live in IO for randomness.
newPassword :: Text -> IO Password
newPassword p = do
    pwd <- (BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy (encodeUtf8 p))
    return (Password (fromMaybe (terror "bcrypt returned NULL") pwd))

checkPassword :: Password -> ByteString -> Bool
checkPassword (Password actual) toCheck =
    BCrypt.validatePassword actual toCheck

instance Prelude.Show Password where
    show _ = "*hidden-password*" :: String


instance FromField Username where
    fromField f (Just bs) = return (Username (decodeUtf8 bs))

instance FromField Password where
    fromField f (Just bs) = return (Password bs)

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


instance ToJSON SSHKey where
    toJSON (SSHKey _ key fingerprint) = object ["key" .= decodeUtf8 key, "fingerprint" .= decodeUtf8 fingerprint]


instance FromField SSHKey where
    fromField f (Just bs) = case parseSSHKey bs of
        Just x -> return x
        _ -> returnError ConversionFailed f "Could not parse ssh key"

instance ToField SSHKey where
    -- Serialize the parsed key (the one we usually use for comparisons etc.)
    toField (SSHKey RSA key _) = Escape ("ssh-rsa " <> key)
    toField (SSHKey DSA key _) = Escape ("ssh-dsa " <> key)

data KeyType = RSA | DSA deriving Show

data SSHKey = SSHKey KeyType ByteString ByteString deriving Show

parseSSHKey :: ByteString -> Maybe SSHKey
parseSSHKey keyData = do
    fp <- fingerprint keyData
    -- keys look like "ssh-rsa AAAAB... ... La2Aw== tom@bla"
    -- but onlyy the middle bit "AAAB ... La2Aw==" is used during checking so we extract that.
    case BS8.split ' ' keyData of
        ["ssh-rsa", k, _] -> Just (SSHKey RSA k fp)
        ["ssh-rsa", k] -> Just  (SSHKey RSA k fp)
        _ -> Nothing
  where
    -- Using unsafeperformIO because fingerprinting is morally a pure
    -- action but we 're usingn ssh-keygen for now.
    fingerprint keyData = unsafePerformIO $ do
        -- e.g. ssh-keygen -l -f /dev/stdin <~/.ssh/id_rsa.pub
        (i, o, e, p) <- runInteractiveCommand "ssh-keygen -l -f /dev/stdin"
        BS.hPut i keyData
        hClose i
        f <- BS.hGetContents o
        return $ case f of
            "" -> Nothing
            x -> Just x
