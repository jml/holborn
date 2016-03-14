-- | Browse is responsible for routing traffic to the holborn-repo
-- server that's responsible for a given repository.
--
-- This is a prime candidate for moving routing decisions closer to
-- the edge but that's a premature optimization for now.
--
-- Test e.g.:
-- $ curl 127.0.0.1:8002/v1/browse -H "content-type: application/json" -d'{"repo": "jml/holborn", "path": ""}'
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Holborn.API.Browse
       ( API
       , server
       ) where

import BasicPrelude

import Data.Aeson (Value(..), object, (.=), FromJSON)
import Servant

import Holborn.API.Types (AppConf(..), Username)
import Holborn.API.Auth (getAuthFromToken)
import Holborn.Auth (AuthToken(..), Permission(..), hasPermission)
import Holborn.Errors (JSONCodeableError(..), APIError(..), jsonErrorHandler)
import Network.Wai (Application, responseLBS)
import Network.HTTP.ReverseProxy (waiProxyTo, defaultOnExc, WaiProxyResponse(WPRModifiedRequest, WPRResponse), ProxyDest(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (status404)
import Holborn.JSON.Browse (BrowseMetaResponse(..))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Network.HTTP.Client (parseUrl, withResponse, Manager, requestHeaders, responseBody)
import Network.HTTP.Types.Header (hAccept)
import Data.ByteString.Lazy (fromStrict)
import Data.Text as T

-- Following imports needed for RPC which we should do in a more
-- clever way (e.g. cereal library will be 100x faster)
import Data.Aeson (decode')
import qualified Data.Time as Time

type Owner = Text
type Repo = Text

type API =
    "v1" :> "repos"
         :> Header "Authorization" AuthToken
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> Get '[JSON] BrowseMetaResponse
    :<|> "v1" :> "repos"
         :> Header "Authorization" AuthToken
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "trees"
         :> Raw
    :<|> "v1" :> "repos"
         :> Header "Authorization" AuthToken
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "blobs"
         :> Raw
    :<|> "v1" :> "repos"
         :> Header "Authorization" AuthToken
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "commits"
         :> Raw

data BrowseError = NotFound

instance JSONCodeableError BrowseError where
    toJSON NotFound = (404, object ["message" .= ("could not find object" :: Text)])


server :: AppConf -> Server API
server conf =
  enter jsonErrorHandler (browse conf)
  :<|> treeCommitBlob conf
  :<|> treeCommitBlob conf
  :<|> treeCommitBlob conf


-- | We're calling repo to get a JSON fragment which we then
-- incorporate into a larger JSON response. We
poorMansJsonGet :: (FromJSON a) => Manager -> Text -> IO (Maybe a)
poorMansJsonGet manager endpoint = do
    r <- parseUrl (T.unpack endpoint)
    let rJson = r { requestHeaders = [(hAccept, "application/json")] }
    resp <- withResponse rJson manager $ \response -> do
      body <- fmap fromStrict (responseBody response)
      return $ decode' body
    pure resp


browse :: AppConf -> Maybe AuthToken -> Owner -> Repo -> ExceptT (APIError BrowseError) IO BrowseMetaResponse
browse AppConf{conn, httpManager} token owner repo = do
    r <- liftIO $ poorMansJsonGet httpManager ("http://127.0.0.1:8080/v1/repos/" <> owner <> "/" <> repo)
    repoMeta <- case r of
        Just x -> pure x
        Nothing -> throwE (SubAPIError NotFound)
    return $ BrowseMetaResponse
      { _BrowseMetaResponse_repo_meta = repoMeta
      , _BrowseMetaResponse_description = "fake description"
      , _BrowseMetaResponse_created_at = Time.LocalTime (Time.ModifiedJulianDay 2000) (Time.TimeOfDay 1 1 1)
      }

-- Tree, commit & blob are passed straight through if they meet the
-- authentication requirements.
treeCommitBlob :: AppConf -> Maybe AuthToken -> Owner -> Repo -> Application
treeCommitBlob AppConf{conn, httpManager} token owner repo =
    waiProxyTo proxy defaultOnExc httpManager
  where
    -- pass the raw request through if the user is authorized
    proxy request = do
        return $ case owner of
            "jml" -> WPRResponse (responseLBS status404 [] "not found")
            _  -> WPRModifiedRequest request (ProxyDest "127.0.0.1" 8080)
