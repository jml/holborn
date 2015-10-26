{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Internal API for e.g. checking whether a user is authorized to
-- access a repository.
--
-- This API is highly specific to our current openssh implementation
-- and has absolutely no stability guarantees.

module Holborn.Api.Internal
       ( API
       , server
       ) where

import BasicPrelude

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON(..), (.=), object)
import Servant ((:>), (:<|>)(..), Post, ReqBody, JSON, ServantErr, Server)
import qualified Data.Map.Strict as DMS
import Control.Monad.Trans.Either (EitherT)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Combinator as AT

import System.IO (stdout, hFlush)

-- | Main internal API (only used by our openssh version ATM).
type API =
    "internal"
        :> "ssh"
        :> "check-key"
        :> ReqBody '[JSON] CheckKeyRequest
        :> Post '[JSON] CheckKeyResponse
    :<|> "internal"
        :> "ssh"
        :> "check-repo-access"
        :> ReqBody '[JSON] CheckRepoAccessRequest
        :> Post '[JSON] CheckRepoAccessResponse

-- | Fake database
db :: Map CheckKeyRequest Text
db = DMS.fromList
     [ (CheckKeyRequest
            { key = "AAAAB3NzaC1yc2EAAAABIwAAAQEAtq8LpgrnFQWpIcK5YdrQNzu22sPrbkHKD83g8v/s7Nu3Omb7h5TLBOZ6DYPSorGMKGjDFqo0witXRagWq95HaA9epFXmhJlO3NTxyTAzIZSzql+oJkqszNpmYY09L00EIplE/YKXPlY2a+sGx3CdJxbglGfTcqf0J2DW4wO2ikZSOXRiLEbztyDwc+TNwYJ3WtzTFWhG/9hbbHGZtpwQl6X5l5d2Mhl2tlKJ/zQYWV1CVXLSyKhkb4cQPkL05enguCQgijuI/WsUE6pqdl4ypziXGjlHAfH+zO06s6EDMQYr50xgYRuCBicF86GF8/fOuDJS5CJ8/FWr16fiWLa2Aw=="
            , key_type = "RSA"
            }, "teh")
     , (CheckKeyRequest
            { key = "AAAAB3NzaC1yc2EAAAADAQABAAACAQC8Wa+xniYeoJQWtpSGIL833jtLqOudcpmSwCzaLMfo/JVF6jwXIVcPQW6GIOGPEA+2B6gqYhISqYGg8zPcP6PLMLxDXFo8MvcyYG8cJD53rmT/ZkNgIJYC7ayBCHGn8DX4Y833Ej6C+2EGEk6btt9XQVaayxFGtI8FrHNzeRDu6SQp/oIOtDvhA3IMjDLjOWMlJlPEJpx6BzGw/v5i5n594DYuq5xDWCsg1KHM5c5z4nGYB4WTM2Ba0zauTCW/VlBxa3gkljqsomU31q9Ylf0Da0/YAt82WwqS2066XO3ea+quR7utkThsTGCEbt00AJZgl5RVr4691W5U4yS8+HRPklFFX30fzcK9FaQvp5da/86MaimdNtGrMVWvOzX26I22SELJvZCL9A4KIHMJS6cc3i6lkxT1L2CKXtvwV+++g1KoPWuoyhlqXazpmXmI5P+Ks5NjT3KkU+oTruiKFIdLNz7ediU06AqAQS4iqOF8o5k6zmB2ojtYLqR+MG4jteWQLO/azbGdxXAO54nGtp1kx3MTEuQTAbaEOkjphQNV5FvJkw2abk2grcEfN2p5oKP3m1en2Liri5GsWHOB/bSnhfvMRkeY5yPIOg2rlu4oiJ7uPThm16XiKPKLEGENmXy/6/sFCNiHXoLN2EakXzcbH7Wl/NEcm9dzvhbP4GO+9w=="
            , key_type = "RSA"
            }, "jml")
     ]

-- | Implementation
checkKey :: CheckKeyRequest -> EitherT ServantErr IO CheckKeyResponse
checkKey r = do
    liftIO $ print ("checkKey", r)
    liftIO $ hFlush stdout
    return $ case DMS.lookup r db of
       Just username -> CheckKeyResponse True username
       _ -> terror "TODO return error for checkKey"


data SSHCommandLine =
      GitReceivePack { orgOrUser :: Text, repo :: Text }
    | GitUploadPack { orgOrUser :: Text, repo :: Text }
    | Invalid Text
    deriving Show

-- There are two acceptable commands:
--   "git-upload-pack '/org/hello'"
--   "git-receive-pack '/org/hello'"
-- For all other commands we can send back futurama quotes.
--
-- TODO TODO - this is a security sensitive piece (gatekeeper for a
-- remote ssh trying to run random commands) and as such it needs
-- quickchecking!
parseSSHCommand :: AT.Parser SSHCommandLine
parseSSHCommand =
    upload <|> receive <|> (fmap Invalid AT.takeText)
  where
    upload = do
        _ <- AT.string "git-upload-pack '"
        (org, user) <- repoPath
        return (GitUploadPack org user)
    receive = do
        _ <- AT.string "git-receive-pack '"
        (org, user) <- repoPath
        return (GitReceivePack org user)
    repoPath = do
        AT.skipWhile (== '/') -- skip optional leading /
        org <- AT.takeWhile1 (/= '/')
        _ <- AT.char '/'
        user <- AT.takeWhile1 (/= '\'')
        _ <- AT.char '\''
        AT.endOfInput
        return (org, user)

checkRepoAccess :: CheckRepoAccessRequest -> EitherT ServantErr IO CheckRepoAccessResponse
checkRepoAccess request = do
    let Right cmd = AT.parseOnly parseSSHCommand (command request)
    liftIO $ print ("checkRepoAccess", request)
    liftIO $ print cmd
    liftIO $ hFlush stdout
    -- TODO - this is where we'd stick actual access controls, rate
    -- limiting etc.

    -- OpenSSH runs the command we send in bash, so we can use common
    -- shell muckery to first send the metadata and then do a
    -- bidirectional pipe.
    return $ case cmd of
        GitReceivePack org repo ->
            CheckRepoAccessResponse True (
                concat ["(echo -n '{\"command\": \"git-receive-pack\", \"org\": \""
                       , org
                       , "\", \"repo\": \""
                       , repo
                       ,"\"}' && cat) | nc 127.0.0.1 8081"
                       ])
        GitUploadPack org repo ->
            CheckRepoAccessResponse True (
                concat ["(echo -n '{\"command\": \"git-upload-pack\", \"org\": \""
                       , org
                       , "\", \"repo\": \""
                       , repo
                       ,"\"}' && cat) | nc 127.0.0.1 8081"
                       ])
        Invalid _ ->
            CheckRepoAccessResponse True (quotes !! 0)
  where
    quotes :: [Text]
    quotes =
        [ "echo 'A meal is a meal'"
        , "echo 'I\'m swelling with patriotic mucus'"
        , "echo 'Look at me. I\'m Dr. Zoidberg, homeowner'"
        ]

server :: Server API
server = checkKey
    :<|> checkRepoAccess

-- Serialization bla bla
data CheckKeyRequest = CheckKeyRequest
    { key :: Text
    , key_type :: Text
    } deriving (Show, Generic, Eq, Ord)
instance FromJSON CheckKeyRequest

data CheckKeyResponse = CheckKeyResponse
    { allowed :: Bool
    , username_ :: Text
    } deriving (Show, Generic)

instance ToJSON CheckKeyResponse where
    toJSON (CheckKeyResponse allowed username_) =
        object [ "allowed"  .= allowed
               , "username" .= username_
               ]

data CheckRepoAccessRequest = CheckRepoAccessRequest
    { username :: Text
    , command :: Text
    } deriving (Show, Generic)
instance FromJSON CheckRepoAccessRequest

data CheckRepoAccessResponse = CheckRepoAccessResponse
    { allowed_ :: Bool
    , target :: Text -- E.g. "nc 127.0.0.1:8080"
    } deriving (Show)

instance ToJSON CheckRepoAccessResponse where
    toJSON (CheckRepoAccessResponse allowed_ target) =
        object [ "allowed" .= allowed_
               , "target"  .= target
               ]
