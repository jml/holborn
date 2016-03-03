{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NamedFieldPuns     #-}

-- | Internal API for e.g. checking whether a user is authorized to
-- access a repository.
--
-- This API is highly specific to our current openssh implementation
-- and has absolutely no stability guarantees.

module Holborn.API.Internal
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
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Types (AppConf(..), parseSSHKey)

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

-- | Implementation
checkKey :: AppConf -> CheckKeyRequest -> EitherT ServantErr IO CheckKeyResponse
checkKey AppConf{conn} request = do
    liftIO $ print ("checkKey", request)
    liftIO $ hFlush stdout
    rows <- liftIO $ query conn [sql|
                   select pk.id, pk.verified
                   from "public_key" as pk  where comparison_pubkey = ?
               |] (Only (key request))
    liftIO $ print rows

    return $ case rows of
       [(keyId, True)] -> CheckKeyResponse (Just keyId)
       [(keyId, False)] -> CheckKeyResponse (Just keyId) -- PUPPY: REMOVE ME key not verified
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
-- PUPPY - this is a security sensitive piece (gatekeeper for a
-- remote ssh trying to run random commands) and as such it needs
-- quickchecking!
parseSSHCommand :: AT.Parser SSHCommandLine
parseSSHCommand =
    upload <|> receive <|> (fmap Invalid AT.takeText)
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
        user <- AT.takeWhile1 (/= '\'')
        void $ AT.char '\''
        AT.endOfInput
        return (org, user)

checkRepoAccess :: AppConf -> CheckRepoAccessRequest -> EitherT ServantErr IO CheckRepoAccessResponse
checkRepoAccess AppConf{conn} request = do
    let Right cmd = AT.parseOnly parseSSHCommand (command request)
    liftIO $ print request
    rows <- liftIO $ query conn [sql|
                   select pk.readonly, pk.verified
                   from "public_key" as pk where id = ?
               |] (Only (key_id request))

    -- OpenSSH runs the command we send in bash, so we can use common
    -- shell muckery to first send the metadata and then do a
    -- bidirectional pipe.
    return $ case (cmd, rows) of
        (GitReceivePack org repo, [(False, True)]) ->
            CheckRepoAccessResponse (Just (
                concat ["(echo -n '{\"command\": \"git-receive-pack\", \"org\": \""
                       , org
                       , "\", \"repo\": \""
                       , repo
                       ,"\"}' && cat) | nc 127.0.0.1 8081"
                       ]))
        (GitReceivePack org repo, [(True, True)]) ->
            CheckRepoAccessResponse (Just "This SSH key is readonly")
        (GitUploadPack org repo, [(_, True)]) ->
            CheckRepoAccessResponse (Just (
                concat ["(echo -n '{\"command\": \"git-upload-pack\", \"org\": \""
                       , org
                       , "\", \"repo\": \""
                       , repo
                       ,"\"}' && cat) | nc 127.0.0.1 8081"
                       ]))
        (_, [(False, _)]) ->
            CheckRepoAccessResponse (Just "SSH key not verified")
        (Invalid _, [(_, _)]) ->
            CheckRepoAccessResponse (Just (quotes !! 0))
  where
    quotes :: [Text]
    quotes =
        [ "echo 'A meal is a meal'"
        , "echo 'I\'m swelling with patriotic mucus'"
        , "echo 'Look at me. I\'m Dr. Zoidberg, homeowner'"
        ]

server :: AppConf -> Server API
server conf = checkKey conf
    :<|> checkRepoAccess conf

-- Serialization bla bla
data CheckKeyRequest = CheckKeyRequest
    { key :: Text
    , key_type :: Text
    } deriving (Show, Generic, Eq, Ord)
instance FromJSON CheckKeyRequest

type KeyId = Int

data CheckKeyResponse = CheckKeyResponse
    { key_id_ :: Maybe KeyId -- TODO might be more useful to return Either with error message?
    } deriving (Show, Generic)

instance ToJSON CheckKeyResponse where
    toJSON (CheckKeyResponse (Just key_id_)) =
        object [ "allowed"  .= True
               , "key_id" .= key_id_
               ]
    toJSON (CheckKeyResponse Nothing) =
        object [ "allowed"  .= False
               ]

data CheckRepoAccessRequest = CheckRepoAccessRequest
    { key_id :: KeyId
    , command :: Text
    } deriving (Show, Generic)
instance FromJSON CheckRepoAccessRequest

data CheckRepoAccessResponse = CheckRepoAccessResponse
    { target :: Maybe Text -- E.g. "nc 127.0.0.1:8080"
    } deriving (Show)

instance ToJSON CheckRepoAccessResponse where
    toJSON (CheckRepoAccessResponse (Just target)) =
        object [ "allowed" .= True
               , "target"  .= target
               ]
    toJSON (CheckRepoAccessResponse Nothing) =
        object [ "allowed" .= False
               ]
