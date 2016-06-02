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

import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (AppConf(..))
import Holborn.API.Internal (APIHandler, JSONCodeableError(..), toServantHandler, throwHandlerError, query)
import Holborn.JSON.NewRepo (NewRepoRequest(..))
import Holborn.JSON.RepoMeta (RepoMeta(..))
import Holborn.API.Auth (getUserId)
import Holborn.API.Types (Username)


type API =
    "new-repo"
    :> Header "GAP-Auth" Username
    :> ReqBody '[JSON] NewRepoRequest
    :> Post '[JSON] RepoMeta


data NewRepoError = AlreadyExists | PermissionDenied | OwnerNotFound

instance JSONCodeableError NewRepoError where
    toJSON AlreadyExists = (400, object ["message" .= ("Repository with this name already exists" :: Text)])
    toJSON PermissionDenied = (400, object ["message" .= ("You are not allowed to create this repository" :: Text)])
    toJSON OwnerNotFound = (400, object ["message" .= ("Owner not found" :: Text)])


server :: AppConf -> Server API
server conf =
  enter (toServantHandler conf) newRepo


newRepo :: Maybe Username -> NewRepoRequest -> APIHandler NewRepoError RepoMeta
newRepo username NewRepoRequest{..} = do
    userId <- getUserId username

    -- TODO user permissions and userId to check whether user is
    -- allowed to create repo.

    -- UNION query to check both user and org at the same time
    result <- query [sql|
        select 'org', id from "org" where orgname = ? UNION select 'user',  id from "user" where username = ?
        |] (_NewRepoRequest_owner, _NewRepoRequest_owner)

    case result of
         [("org" :: String, orgId :: Int)] -> newOrgRepo orgId
         [("user" :: String, userId' :: Int)] -> newUserRepo userId'
         [] -> throwHandlerError OwnerNotFound
         _ -> terror "Unexpected number of rows in newRepo"

  where
    newOrgRepo orgId = do
       [Only (repoId :: Int)] <- query [sql|
            insert into "org_repo" (name, description, org_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (_NewRepoRequest_name, _NewRepoRequest_description, orgId, "127.0.0.1:8080" :: Text)
       pure (RepoMeta _NewRepoRequest_owner _NewRepoRequest_name 0 0 0)

    newUserRepo userId = do
       [Only (repoId :: Int)] <- query [sql|
            insert into "user_repo" (name, description, user_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (_NewRepoRequest_name, _NewRepoRequest_description, userId, "127.0.0.1:8080" :: Text)
       pure (RepoMeta _NewRepoRequest_owner _NewRepoRequest_name 0 0 0)
