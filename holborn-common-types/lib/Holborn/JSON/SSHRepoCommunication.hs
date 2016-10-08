-- | Common types for SSH.

{-# LANGUAGE DeriveGeneric #-}

module Holborn.JSON.SSHRepoCommunication
       ( GitCommand(..)
       , unparseGitCommand
       , RepoCall(..)
       , SSHKey(SSHKey)
       , KeyType(..)
       , parseSSHCommand
       , unparseSSHCommand
       , parseSSHKey
       , unparseSSHKey
       , parseKeyType
       , unparseKeyType
       , SSHCommandLine(..)
       ) where

import HolbornPrelude

import Control.Applicative (Alternative(..))
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(Object)
  , genericParseJSON
  , genericToJSON
  , object
  , withText
  , (.=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(ConversionFailed))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(Escape))
import GHC.Generics (Generic)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafePerformIO) -- Temporary hack until we have a pure fingerprinter
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (Arbitrary(..), elements)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Web.HttpApiData (toUrlPiece)

import Holborn.CommonTypes.Repo
  ( OwnerName
  , ownerNameParser
  , RepoName
  , repoNameParser
  , RepoId
  )


-- | Git offers two kinds of service.
data GitCommand = GitUploadPack | GitReceivePack deriving (Eq, Show, Generic)

gitCommandParser :: AT.Parser GitCommand
gitCommandParser = ("git-upload-pack" *> pure GitUploadPack) <|> ("git-receive-pack" *> pure GitReceivePack)

unparseGitCommand :: IsString string => GitCommand -> string
unparseGitCommand serviceType =
  case serviceType of
    GitUploadPack -> "git-upload-pack"
    GitReceivePack -> "git-receive-pack"

instance ToHttpApiData GitCommand where
    toUrlPiece = unparseGitCommand

instance FromHttpApiData GitCommand where
  -- | Turn data from HTTP into a GitCommand.
  --
  -- e.g.
  -- > parseUrlPiece "git-upload-pack"
  -- Right GitUploadPack
  --
  -- > parseUrlPiece "git-receive-pack"
  -- Right GitReceivePack
  --
  -- > parseUrlPiece "sandwiches"
  -- Left "Parse error: sandwiches"
  parseUrlPiece = fmapL fromString . AT.parseOnly gitCommandParser

instance Arbitrary GitCommand where
    arbitrary = elements [ GitReceivePack, GitUploadPack ]

instance FromJSON GitCommand
instance ToJSON GitCommand

-- | A user-generated request to interact with a git repository.
data SSHCommandLine =
  SSHCommandLine { gitCommand :: GitCommand
                 , ownerName :: OwnerName
                 , repoName :: RepoName
                 } deriving (Show, Eq)

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
sshCommand = do
  command <- gitCommandParser
  void $ AT.string " '"
  AT.skipWhile (== '/') -- skip optional leading /
  org <- ownerNameParser
  void $ AT.char '/'
  repoName <- repoNameParser
  void $ AT.char '\''
  AT.endOfInput
  pure $ SSHCommandLine command org repoName


parseSSHCommand :: Alternative m => Text -> m SSHCommandLine
parseSSHCommand = hush . AT.parseOnly sshCommand

unparseSSHCommand :: SSHCommandLine -> Text
unparseSSHCommand (SSHCommandLine command owner repo) =
  unparseGitCommand command <> " '" <> toUrlPiece owner <> "/" <> toUrlPiece repo <> "'"

instance Arbitrary SSHCommandLine where
  arbitrary = SSHCommandLine <$> arbitrary <*> arbitrary <*> arbitrary


-- | Permission to interact with a git repository.
data RepoCall =
      WritableRepoCall { _command  :: GitCommand, _repoId :: RepoId }
    deriving (Eq, Show, Generic)

instance FromJSON RepoCall where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop (length ("_" :: String))}

instance ToJSON RepoCall where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop (length ("_" :: String))}

instance Arbitrary RepoCall where
  arbitrary = WritableRepoCall <$> arbitrary <*> arbitrary


-- | SSH key type
data KeyType = RSA | DSA deriving (Show, Eq, Ord, Read)

instance Arbitrary KeyType where
  arbitrary = elements [ RSA, DSA ]

-- TODO: No reason to have this different to parseKeyType and unparseKeyType.
instance FromJSON KeyType where
    parseJSON "RSA" = pure RSA
    parseJSON "DSA" = pure DSA
    parseJSON _ = mzero

instance ToJSON KeyType where
  toJSON RSA = "RSA"
  toJSON DSA = "DSA"

instance ToField KeyType where
  toField RSA = Escape "RSA"
  toField DSA = Escape "DSA"

instance FromField KeyType where
  fromField _ (Just "RSA") = pure RSA
  fromField _ (Just "DSA") = pure DSA
  fromField f x            = returnError ConversionFailed f ("Invalid key type in database: " <> (textToString (show x)))

keyTypeParser :: AB.Parser KeyType
keyTypeParser = ("ssh-rsa" *> pure RSA) <|> ("ssh-dss" *> pure DSA)

parseKeyType :: Alternative m => ByteString -> m KeyType
parseKeyType = hush . AB.parseOnly keyTypeParser

unparseKeyType :: IsString s => KeyType -> s
unparseKeyType RSA = "ssh-rsa"
unparseKeyType DSA = "ssh-dss"


-- | An SSH key.
data SSHKey = SSHKey { _sshKeyType :: KeyType
                     , _sshKeyData :: ByteString
                     , _sshKeyComment :: Maybe ByteString
                     } deriving (Show, Eq)

instance ToJSON SSHKey where
    toJSON key = object
      [ "key"         .= (decodeUtf8 $ _sshKeyData key)
      , "comment"     .= (decodeUtf8 <$> _sshKeyComment key)
      , "type"        .= toJSON (_sshKeyType key)
      ]

instance FromJSON SSHKey where
    parseJSON (Object v) = SSHKey <$> v .: "type"
                                  <*> (encodeUtf8 <$> v .: "key")
                                  <*> (map encodeUtf8 <$> v .:? "comment")
    parseJSON wat = typeMismatch "SSHKey" wat

-- TODO: We might want to lock down SSHKey so we don't expose the construct
-- and the only way to get a valid one is to parse it via parseSSHKey or load
-- it from the database (and we have code to ensure that we only store valid
-- keys in the database).

-- | Decode an SSHKey from the submitted_key field in the database.
instance FromField SSHKey where
    fromField f (Just bs) = case parseSSHKey bs of
        Just x -> return x
        _ -> returnError ConversionFailed f ("Invalid SSH key in database: " <> textToString (show bs))

    -- TODO: Move corruptDatabase to holborn-prelude and use that
    fromField _ _ = error "FromField SSHKey should always decode correctly"

instance ToField SSHKey where
    toField = Escape . unparseSSHKey

-- | Decode an SSH from the decomposed columns in the database.
instance FromRow SSHKey where
    fromRow = SSHKey <$> field <*> field <*> field


-- | Generate an SSH fingerprint.
sshFingerprint :: Alternative m => ByteString -> IO (m ByteString)
sshFingerprint keyData = do
  -- Not all ssh-keygen's handle /dev/stdin correctly. Write to a temporary
  -- file instead.
  keyPath <- withTempFile "ssh-key" $ \path h -> do
    BS.hPut h keyData
    pure path
  (exitCode, out, err) <- readProcessWithExitCode "ssh-keygen" ["-l", "-f", keyPath] ""
  case exitCode of
    ExitFailure 127 -> error "Could not find ssh-keygen process"
    ExitFailure errCode -> do
      putStrLn $ "ssh-keygen failed: (" <> show errCode <>  ")"
      putStrLn $ "output:\n" <> fromString out
      putStrLn $ "error:\n" <> fromString err
      pure empty
    ExitSuccess -> pure (pure (fromString out))

  where
    withTempFile name action = do
      tempDir <- getTemporaryDirectory
      bracket (openTempFile tempDir name) (hClose . snd) (uncurry action)

-- | Parse a single SSH key, checking for validity.
parseSSHKey :: ByteString -> Maybe SSHKey
parseSSHKey keyData = do
  (keyType, key, comment) <- hush (AB.parseOnly sshKeyParser keyData)
  -- Generate the fingerprint so we *know* this is a valid key.
  -- Using unsafeperformIO because fingerprinting is morally a pure
  -- action but we 're using ssh-keygen for now.
  -- TODO: Check that type & comment returned by sshFingerprint match type &
  -- comment in key.
  void $ unsafePerformIO $ sshFingerprint keyData
  pure $ SSHKey keyType key comment

-- | Parser for a single SSH key
--
-- e.g.
--  ssh-rsa AAAAbc22...  zoidberg@space
--
-- Doesn't check the semantic validity of the key, just does the basic syntax.
sshKeyParser :: AB.Parser (KeyType, ByteString, Maybe ByteString)
sshKeyParser = do
  keyType <- keyTypeParser
  void $ AB.string " "
  key <- AB.takeWhile1 (AB.notInClass " ")
  comment <- AB.option Nothing $ do
    void $ AB.string " "
    Just <$> AB.takeWhile1 (AB.notInClass "\n")
  pure (keyType, key, comment)

-- | Serialize the parsed key (the one we usually use for comparisons etc.)
unparseSSHKey :: SSHKey -> ByteString
unparseSSHKey (SSHKey keyType key comment) =
  unparseKeyType keyType <> " " <> key <> maybe mempty (" " <>) comment
