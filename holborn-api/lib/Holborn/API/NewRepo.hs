-- | Create new repositories for either a user or an org
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}


module Holborn.API.NewRepo
       ( API
       , server
       ) where

import HolbornPrelude

import Data.Aeson (object, (.=))
import Servant

import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , pickRepoServer
  , toServantHandler
  , throwHandlerError
  , query
  )
import Holborn.JSON.NewRepo (NewRepoRequest(..))
import Holborn.JSON.RepoMeta (RepoMeta(..), OwnerName, newOwnerName)
import Holborn.API.Types (DexMail)
import Holborn.API.Auth (getUserId)

type API =
    "create-repository"
    :> Header "x-dex-email" DexMail
    :> ReqBody '[JSON] NewRepoRequest
    :> Post '[JSON] RepoMeta
  :<|> "user" :> "repository-owner-candidates" -- TODO should this be here
    :> Header "x-dex-email" DexMail
    :> Get '[JSON] [OwnerName]


data NewRepoError = AlreadyExists | PermissionDenied | OwnerNotFound

instance JSONCodeableError NewRepoError where
    toJSON AlreadyExists = (400, object ["message" .= ("Repository with this name already exists" :: Text)])
    toJSON PermissionDenied = (400, object ["message" .= ("You are not allowed to create this repository" :: Text)])
    toJSON OwnerNotFound = (400, object ["message" .= ("Owner not found" :: Text)])


server :: Config -> Server API
server conf =
  enter (toServantHandler conf) (newRepo :<|> repoOwnerCandidates)


newRepo :: Maybe DexMail -> NewRepoRequest -> APIHandler NewRepoError RepoMeta
newRepo _dexMail NewRepoRequest{..} = do
    -- TODO user permissions and userId to check whether user is
    -- allowed to create repo.

    -- UNION query to check both user and org at the same time
    result <- query [sql|
        select 'org', id, orgname from "org" where orgname = ? UNION select 'user', id, username from "user" where username = ?
        |] (owner, owner)

    case result of
         [("org" :: String, orgId :: Int, owner' :: OwnerName)] -> newOrgRepo orgId owner'
         [("user" :: String, userId' :: Int, owner' :: OwnerName)] -> newUserRepo userId' owner'
         [] -> throwHandlerError OwnerNotFound
         _ -> terror "Unexpected number of rows in newRepo"

  where
    newOrgRepo orgId owner' = do
       repoServer <- pickRepoServer
       [Only (repoId :: Int)] <- query [sql|
            insert into "org_repo" (name, description, org_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (name, description, orgId, repoServer)
       pure (RepoMeta repoId 0 0 0 owner')

    newUserRepo userId owner' = do
       repoServer <- pickRepoServer
       [Only (repoId :: Int)] <- query [sql|
            insert into "user_repo" (name, description, user_id, hosted_on) values (?, ?, ?, ?) returning id
            |] (name, description, userId, repoServer)
       pure (RepoMeta repoId 0 0 0 owner')


repoOwnerCandidates :: Maybe DexMail -> APIHandler NewRepoError [OwnerName]
repoOwnerCandidates dexMail = do
  userId <- getUserId dexMail
  let Just name = newOwnerName (show userId)
  pure [name]
