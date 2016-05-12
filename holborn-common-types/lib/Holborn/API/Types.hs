module Holborn.API.Types
       ( newUsername
       , newEmail
       , newPassword
       , checkPassword
       , Username
       , Email
       , Password
       , ApiError(..)
       , SSHKey
       , KeyType(..)
       , parseSSHKey
       ) where

import BasicPrelude hiding (empty)
import Control.Applicative (Alternative(..))
import qualified Crypto.BCrypt as BCrypt
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(ConversionFailed))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(Escape))
import qualified Prelude
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=))
import Data.Aeson.Types (typeMismatch)
import System.Process (runInteractiveCommand)
import System.IO.Unsafe (unsafePerformIO) -- Temporary hack until we have a pure fingerprinter
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.IO (hClose)

newtype Username = Username Text deriving (Eq, Ord, Show, ToField)
newtype Email = Email Text deriving (Eq, Ord, Show, ToField)
newtype Password = Password ByteString deriving (ToField)


-- TODO: Validate username
newUsername :: Text -> Username
newUsername = Username

newEmail :: Text -> Email
newEmail = Email

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


-- | SSH key type
data KeyType = RSA | DSA deriving (Show, Eq, Ord)


parseKeyType :: (Alternative m, Eq s, IsString s) => s -> m KeyType
parseKeyType "ssh-rsa" = pure RSA
parseKeyType "ssh-dsa" = pure DSA
parseKeyType _ = empty


unparseKeyType :: IsString s => KeyType -> s
unparseKeyType RSA = "ssh-rsa"
unparseKeyType DSA = "ssh-dsa"


instance FromJSON KeyType where
    parseJSON "RSA" = pure RSA
    parseJSON "DSA" = pure DSA
    parseJSON _ = mzero


data SSHKey = SSHKey { _sshKeyType :: KeyType
                     , _sshKeyData :: ByteString
                     , _sshKeyComment :: Maybe ByteString
                     , _sshKeyFingerPrint :: ByteString
                     } deriving Show


instance ToJSON SSHKey where
    -- TODO: Maybe include comment in JSON output?
    toJSON (SSHKey _ key _comment fingerprint) = object ["key" .= decodeUtf8 key, "fingerprint" .= decodeUtf8 fingerprint]


instance FromField SSHKey where
    fromField f (Just bs) = case parseSSHKey bs of
        Just x -> return x
        _ -> returnError ConversionFailed f "Could not parse ssh key"
    fromField _ _ = terror "FromField SSHKey should always decode correctly"


instance ToField SSHKey where
    toField = Escape . unparseSSHKey


instance FromRow SSHKey where
    fromRow = field


-- TODO: parse & unparse are obvious candidates for tests
parseSSHKey :: MonadPlus m => ByteString -> m SSHKey
parseSSHKey keyData = do
  -- keys look like "ssh-rsa AAAAB... ... La2Aw== tom@bla"
  -- but onlyy the middle bit "AAAB ... La2Aw==" is used during checking so we extract that.
  -- TODO: We could totally do this with just applicatives but it'd be way too
  -- cumbersome. Just switch to attoparsec instead
  (keyType, key, comment) <- case BS8.split ' ' keyData of
                               [kt, k, c] -> pure (kt, k, Just c)
                               [kt, k] -> pure (kt, k, Nothing)
                               _ -> empty
  SSHKey <$> parseKeyType keyType <*> pure key <*> pure comment <*> fingerprint keyData
  where

    -- Using unsafeperformIO because fingerprinting is morally a pure
    -- action but we 're using ssh-keygen for now.
    fingerprint keyData' = unsafePerformIO $ do
        -- e.g. ssh-keygen -l -f /dev/stdin <~/.ssh/id_rsa.pub
        (i, o, _, _) <- runInteractiveCommand "ssh-keygen -l -f /dev/stdin"
        BS.hPut i keyData'
        hClose i
        f <- BS.hGetContents o
        return $ case f of
            "" -> empty
            x -> pure x


-- | Serialize the parsed key (the one we usually use for comparisons etc.)
unparseSSHKey :: SSHKey -> ByteString
unparseSSHKey (SSHKey keyType key comment _) =
  unparseKeyType keyType <> " " <> key <> maybe mempty (" " <>) comment
