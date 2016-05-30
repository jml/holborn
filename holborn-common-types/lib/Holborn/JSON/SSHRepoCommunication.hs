-- | Common types for SSH.

{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.SSHRepoCommunication
       ( RepoCall(..)
       , SSHKey
       , KeyType(..)
       , parseSSHCommand
       , unparseSSHCommand
       , parseSSHKey
       , unparseSSHKey
       , SSHCommandLine(..)
       , RepoAccess(..)
       ) where

import BasicPrelude hiding (empty)

import Control.Applicative (Alternative(..))
import Control.Error (rightZ)
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, object, withText, (.=))
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(ConversionFailed))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(Escape))
import GHC.Generics (Generic)
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO) -- Temporary hack until we have a pure fingerprinter
import System.Process (runInteractiveCommand)
import Holborn.JSON.RepoMeta (ValidRepoName, newValidRepoName)


import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , elements
  , listOf1
  , oneof
  )



-- | A user-generated request to interact with a git repository.
data SSHCommandLine =
      GitReceivePack { _orgOrUser :: Text, _sshCommandLineRepo :: ValidRepoName }
    | GitUploadPack { _orgOrUser :: Text, _sshCommandLineRepo :: ValidRepoName }
    deriving (Show, Eq)

instance FromJSON SSHCommandLine where
  parseJSON = withText "SSH command must be text" parseSSHCommand

instance ToJSON SSHCommandLine where
  toJSON = toJSON . unparseSSHCommand


-- There are two acceptable commands:
--   "git-upload-pack '/org/hello'"
--   "git-receive-pack '/org/hello'"
-- For all other commands we can send back futurama quotes.
--
-- PUPPY - this is a security sensitive piece (gatekeeper for a
-- remote ssh trying to run random commands) and as such it needs
-- quickchecking!
sshCommand :: AT.Parser SSHCommandLine
sshCommand =
    upload <|> receive
  where
    upload = do
        void $ AT.string "git-upload-pack '"
        uncurry GitUploadPack <$> repoPath
    receive = do
        void $ AT.string "git-receive-pack '"
        uncurry GitReceivePack <$> repoPath
    repoPath = do
        AT.skipWhile (== '/') -- skip optional leading /
        org <- AT.takeWhile1 (/= '/')
        void $ AT.char '/'
        repoName <- AT.takeWhile1 (/= '\'')
        void $ AT.char '\''
        AT.endOfInput

        case newValidRepoName repoName of
          Nothing -> fail "Invalid repository name"
          Just x -> pure (org, x)


parseSSHCommand :: MonadPlus m => Text -> m SSHCommandLine
parseSSHCommand = rightZ . AT.parseOnly sshCommand

unparseSSHCommand :: SSHCommandLine -> Text
unparseSSHCommand (GitReceivePack owner repo) = "git-receive-pack '" <> owner <> "/" <> show repo <> "'"
unparseSSHCommand (GitUploadPack owner repo) = "git-upload-pack '" <> owner <> "/" <> show repo <> "'"


instance Arbitrary SSHCommandLine where
  arbitrary =
    constructor <*> pathSegment <*> arbitrary
    where
      constructor = elements [ GitReceivePack, GitUploadPack ]

-- | Used to generate arbitrary valid owners and repository names.
-- TODO: Encode this assumption in the types of owner, repo, etc.
pathSegment :: Gen Text
pathSegment =
  fromString <$> listOf1 (elements alphabet)
  where
    alphabet = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> "-_."


-- | Permission to interact with a git repository.
data RepoCall =
      WritableRepoCall { _command :: SSHCommandLine }
    | ImplicitRepoCall { _command :: SSHCommandLine, _owner :: Text }
    deriving (Eq, Show, Generic)

instance FromJSON RepoCall where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop (length ("_" :: String))}

instance ToJSON RepoCall where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop (length ("_" :: String))}

instance Arbitrary RepoCall where
  arbitrary =
    oneof [ WritableRepoCall <$> arbitrary
          , ImplicitRepoCall <$> arbitrary <*> pathSegment
          ]


-- | Routing to a git repository.
data RepoAccess = AccessGranted Hostname Port RepoCall deriving (Show)

type Hostname = Text
type Port = Int


-- | SSH key type
data KeyType = RSA | DSA deriving (Show, Eq, Ord)

instance FromJSON KeyType where
    parseJSON "RSA" = pure RSA
    parseJSON "DSA" = pure DSA
    parseJSON _ = mzero


parseKeyType :: (Alternative m, Eq s, IsString s) => s -> m KeyType
parseKeyType "ssh-rsa" = pure RSA
parseKeyType "ssh-dsa" = pure DSA
parseKeyType _ = empty


unparseKeyType :: IsString s => KeyType -> s
unparseKeyType RSA = "ssh-rsa"
unparseKeyType DSA = "ssh-dsa"


-- | An SSH key.
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
