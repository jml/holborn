-- | Create new repositories for either a user or an org
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}

module Holborn.API.NewRepo
       ( API
       , server
       ) where

import BasicPrelude

import Data.Aeson (object, (.=))
import Servant

import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (AppConf(..))
import Holborn.Auth (AuthToken(..))
import Holborn.Errors (JSONCodeableError(..), APIError(..), jsonErrorHandler)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Holborn.JSON.NewRepo (NewRepoRequest(..))
import Holborn.JSON.RepoMeta (RepoMeta(..))
import Holborn.API.Auth (getAuthFromToken)


type API =
    "new-repo"
    :> Header "Authorization" AuthToken
    :> ReqBody '[JSON] NewRepoRequest
    :> Post '[JSON] RepoMeta


data NewRepoError = AlreadyExists | PermissionDenied | OwnerNotFound

instance JSONCodeableError NewRepoError where
    toJSON AlreadyExists = (400, object ["message" .= ("Repository with this name already exists" :: Text)])
    toJSON PermissionDenied = (400, object ["message" .= ("You are not allowed to create this repository" :: Text)])
    toJSON OwnerNotFound = (400, object ["message" .= ("Owner not found" :: Text)])


server :: AppConf -> Server API
server conf =
  enter jsonErrorHandler (newRepo conf)


newRepo :: AppConf -> Maybe AuthToken -> NewRepoRequest -> ExceptT (APIError NewRepoError) IO RepoMeta
newRepo appconf@AppConf{conn} token NewRepoRequest{..} = do
    (userId, permissions) <- getAuthFromToken appconf token

    -- TODO user permissions and userId to check whether user is
    -- allowed to create repo.

    -- UNION query to check both user and org at the same time
    result <- liftIO $ query conn [sql|
        select 'org', id from "org" where orgname = ? UNION select 'user',  id from "user" where username = ?
        |] (_NewRepoRequest_owner, _NewRepoRequest_owner)

    case result of
         [("org" :: String, orgId :: Int)] -> newOrgRepo orgId
         [("user" :: String, userId' :: Int)] -> newUserRepo userId'
         [] -> throwE (SubAPIError OwnerNotFound)
         _ -> terror "Unexpected number of rows in newRepo"

  where
    newOrgRepo orgId = do
       [Only (repoId :: Int)] <- liftIO $ query conn [sql|
            insert into "org_repo" (name, description, org_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (_NewRepoRequest_name, _NewRepoRequest_description, orgId, "127.0.0.1:8080" :: Text)
       pure (RepoMeta _NewRepoRequest_owner _NewRepoRequest_name 0 0 0)

    newUserRepo userId = do
       [Only (repoId :: Int)] <- liftIO $ query conn [sql|
            insert into "user_repo" (name, description, user_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (_NewRepoRequest_name, _NewRepoRequest_description, userId, "127.0.0.1:8080" :: Text)
       pure (RepoMeta _NewRepoRequest_owner _NewRepoRequest_name 0 0 0)
