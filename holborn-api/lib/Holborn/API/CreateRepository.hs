-- | Create new repositories for either a user or an org
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}


module Holborn.API.CreateRepository
       ( API
       , server
       , CreateRepositoryResponse(..) -- to make warnings shut up
       ) where

import HolbornPrelude hiding (id)

import Control.Monad (fail)
import Data.Aeson (FromJSON(..), Value(String), object, (.=), ToJSON(..))
import Data.Aeson.Types (typeMismatch)
import Servant

import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , pickRepoServer
  , toServantHandler
  , throwHandlerError
  , query
  )
import Holborn.CommonTypes.Repo (OwnerName, RepoName)
import Holborn.API.Types (DexMail)
import Holborn.API.Auth (getUserId)

data CreateRepositoryResponse =
  CreateRepositoryResponse { id :: Int } deriving (Show, Generic)
instance ToJSON CreateRepositoryResponse

type API =
    "create-repository"
    :> Header "x-dex-email" DexMail
    :> ReqBody '[JSON] CreateRepositoryRequest
    :> Post '[JSON] CreateRepositoryResponse
  -- When creating a repository we need an owners. The following API
  -- call returns a list of valid owner names for a logged in user.
  -- TODO: this might be better off in a different file.
  :<|> "user" :> "repository-owner-candidates"
    :> Header "x-dex-email" DexMail
    :> Get '[JSON] [OwnerName]


-- | Repos are either public or private
-- if private then read-access is controlled via organisation features.
data Visibility = Public | Private deriving (Show, Eq)

-- TODO ToJSON instance for Visibility

instance FromJSON Visibility where
 parseJSON (String v) = case v of
   "public" -> pure Public
   "private" -> pure Private
   _ -> fail ("invalid value: " <> (toS v))
 parseJSON invalid = typeMismatch "Visibility" invalid


data CreateRepositoryRequest = CreateRepositoryRequest
    { owner :: OwnerName
    , name :: RepoName
    , description :: Maybe Text -- Optional
    , visibility :: Visibility
    } deriving (Show, Generic)


instance FromJSON CreateRepositoryRequest


data CreateRepositoryError = AlreadyExists | PermissionDenied | OwnerNotFound

instance JSONCodeableError CreateRepositoryError where
    toJSON AlreadyExists = (400, object ["message" .= ("Repository with this name already exists" :: Text)])
    toJSON PermissionDenied = (400, object ["message" .= ("You are not allowed to create this repository" :: Text)])
    toJSON OwnerNotFound = (400, object ["message" .= ("Owner not found" :: Text)])


server :: Config -> Server API
server conf =
  enter (toServantHandler conf) (newRepo :<|> repoOwnerCandidates)


newRepo :: Maybe DexMail -> CreateRepositoryRequest -> APIHandler CreateRepositoryError CreateRepositoryResponse
newRepo _dexMail CreateRepositoryRequest{..} = do
    -- TODO user permissions and userId to check whether user is
    -- allowed to create repo.

    -- UNION query to check both user and org at the same time
    result <- query [sql|
        select 'org', id, name from "core_org" where name = ? UNION select 'user', id, username from auth_user where username = ?
        |] (owner, owner)

    case result of
         [("org" :: String, orgId :: Int, owner' :: OwnerName)] -> newOrgRepo orgId owner'
         [("user" :: String, userId' :: Int, owner' :: OwnerName)] -> newUserRepo userId' owner'
         [] -> throwHandlerError OwnerNotFound
         _ -> error "Unexpected number of rows in newRepo"

  where
    newOrgRepo orgId owner' = do
       repoServer <- pickRepoServer
       [Only (repoId :: Int)] <- query [sql|
            insert into core_orgrepo (name, description, org_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (name, description, orgId, repoServer)
       pure (CreateRepositoryResponse repoId)

    newUserRepo userId owner' = do
       repoServer <- pickRepoServer
       [Only (repoId :: Int)] <- query [sql|
            insert into core_userrepo (name, description, user_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (name, description, userId, repoServer)
       pure (CreateRepositoryResponse repoId)


repoOwnerCandidates :: Maybe DexMail -> APIHandler CreateRepositoryError [OwnerName]
repoOwnerCandidates dexMail = do
  userId <- getUserId dexMail
  [Only (ownerName :: OwnerName) ] <- query [sql|
            select username from auth_user where id = ?
            |] (Only userId)
  -- TODO link in potential owners when we have an org structure
  pure [ownerName]
