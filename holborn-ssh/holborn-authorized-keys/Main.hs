{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | holborn-authorized-keys: generate authorized_keys for SSH server
--
-- This is used to look up a user's public keys.
--
-- e.g.
--
--   holborn-authorized-keys --key="%k" --type="%t" \
--     --api-url=http://localhost:8002/ \
--     --command=holborn-connect-repo
--
-- openssh can provide us with the following information:
-- * username being authenticated (%u)
-- * home directory of user being authenticated (%h)
-- * key type being offered for authentication (%t)
-- * fingerprint of that key (%f)
-- * the key itself (%k)
--
-- We want to allow holborn users to log in, who will not have shell accounts
-- on our SSH servers (openssh only allows "users" who are also system users).
-- Therefore, we disregard username and validate users based on their key.
--
-- holborn-authorized-keys produces zero or more lines of output in
-- authorized_keys format.
--
-- If the supplied credentials are invalid, we produce no lines. If they are
-- valid, then we produce output like the following:
--
--     <options> <keytype> <base64-encoded key> <comment>
--
-- Where <options> at least includes "restrict" (which makes sure they can't
-- muck about on our servers), and a "command" which indicates what will
-- *actually* get run when users log in. That command can be specified using
-- the `--command` option. The given command will be run without arguments,
-- but will have SSH_ORIGINAL_COMMAND, HOLBORN_KEY_ID, and HOLBORN_API_URL in
-- the environment.
--
-- When deployed, this executable must be:
-- * owned by root
-- * not writable by group or others
-- * specified by an absolute path in the sshd_config

module Main (main) where

import HolbornPrelude
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.ByteString.Char8 as B
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Options.Applicative
  ( ParserInfo
  , ReadM
  , eitherReader
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , str
  , value
  )
import Servant.API ((:>), Post, ReqBody, JSON)
import Servant.Client (ServantError, client, BaseUrl(..), parseBaseUrl, showBaseUrl)
import System.Exit (ExitCode(..), exitWith)
import Holborn.JSON.SSHRepoCommunication
  ( KeyType(..)
  , SSHKey(..)
  , parseKeyType
  , unparseKeyType
  )
import Holborn.API.SSH
  ( CheckKeyRequest(..)
  , KeyId
  , SSHKeys
  , unSSHKeys
  )

-- XXX: Shouldn't have to copy this from Holborn.API.SSH
type API = "internal" :> "ssh" :> "authorized-keys" :> ReqBody '[JSON] CheckKeyRequest :> Post '[JSON] SSHKeys

api :: Proxy API
api = Proxy

getAuthorizedKeys :: CheckKeyRequest -> Manager -> BaseUrl -> ExceptT ServantError IO SSHKeys
getAuthorizedKeys = client api


data Config =
  Config
  { key :: Text
  , keyType :: KeyType
  , apiUrl :: BaseUrl
  , command :: ByteString
  } deriving (Show)

url :: ReadM BaseUrl
url = eitherReader parseUrl
  where
    parseUrl :: String -> Either String BaseUrl
    parseUrl s = fmapL (textToString . show) (parseBaseUrl s)

defaultApiUrl :: BaseUrl
defaultApiUrl = fromMaybe (terror "Default URL is broken") (parseBaseUrl "http://localhost:8002")


options :: ParserInfo Config
options =
  info (helper <*> parser) description
  where
    parser =
      Config
      <$> option (fromString <$> str)
          ( long "key" <> short 'k' <> metavar "KEY"
            <> help "SSH key offered for authentication" )
      <*> option (str >>= parseKeyType . fromString)
          ( long "type" <> short 't' <> metavar "KEY_TYPE"
            <> help "Type of key being offered for authentication" )
      <*> option url
          ( long "api-url" <> metavar "API" <> help "URL of holborn-api server"
            <> value defaultApiUrl )
      <*> option (encodeUtf8 . fromString <$> str)
          ( long "command" <> metavar "COMMAND" <> value "holborn-connect-repo"
            <> help "Command run on authentication, using SSH 'command' option. Will be executed with --key-id=<KEYID> if authentication succeeds." )

    description = concat
      [ fullDesc
      , progDesc "Get authorized_keys file for a holborn user"
      , header "holborn-authorized-keys - generate authorized_keys"
      ]

getKeyRequest :: Config -> CheckKeyRequest
getKeyRequest Config{key, keyType} = CheckKeyRequest { key, keyType }


formatKey :: BaseUrl -> ByteString -> KeyId -> SSHKey -> ByteString
formatKey baseUrl command keyId (SSHKey keyType keyData comment) =
  sshOptions <> " " <> unparseKeyType keyType <> " " <> keyData <> (maybe "" (" " <>) comment)
  where
    sshOptions = intercalate "," [ "restrict", "command=\"" <> command' <> "\"" ]
    envVars = [("HOLBORN_KEY_ID", encodeUtf8 (show keyId))
              ,("HOLBORN_API_URL", encodeUtf8 (fromString (showBaseUrl baseUrl)))]
    command' = intercalate " " [name <> "=" <> value' | (name, value') <- envVars] <> " " <> command


main :: IO ()
main = do
  config <- execParser options
  let request = getKeyRequest config
  manager <- newManager defaultManagerSettings
  result <- runExceptT $ getAuthorizedKeys request manager (apiUrl config)
  case result of
    Left err -> do
      printErr $ "ERROR: " <> show err
      exitWith (ExitFailure 1)
    Right keys -> do
      forM_ (unSSHKeys keys) $
        B.putStrLn . (\(keyId, key) -> formatKey (apiUrl config) (command config) keyId key)
