-- | Browse is responsible for routing traffic to the holborn-repo
-- server that's responsible for a given repository.
--
-- This is a prime candidate for moving routing decisions closer to
-- the edge but that's a premature optimization for now.
--
-- Test e.g.:
-- $ curl 127.0.0.1:8002/v1/browse -H "content-type: application/json" -d'{"repo": "jml/holborn", "path": ""}'
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}

module Holborn.API.Browse
       ( API
       , server
       ) where

import BasicPrelude

import Data.Aeson (object, (.=), FromJSON, decode')
import Servant

import Holborn.API.Config (AppConf(..))
import Holborn.API.Types (Username)
import Holborn.Errors (JSONCodeableError(..), APIError(..), jsonErrorHandler)
import Network.Wai (Application, responseLBS)
import Network.HTTP.ReverseProxy (waiProxyTo, defaultOnExc, WaiProxyResponse(WPRModifiedRequest, WPRResponse), ProxyDest(..))
import Network.HTTP.Types.Status (status404)
import Holborn.JSON.Browse (BrowseMetaResponse(..))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Network.HTTP.Client (parseUrl, withResponse, Manager, requestHeaders, responseBody)
import Network.HTTP.Types.Header (hAccept)
import Data.ByteString.Lazy (fromStrict)
import Data.Text as T

-- Following imports needed for RPC which we should do in a more
-- clever way (e.g. cereal library will be 100x faster)
import qualified Data.Time as Time

type Owner = Text
type Repo = Text

type API =
         Header "GAP-Auth" Username
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> Get '[JSON] BrowseMetaResponse
    :<|> Header "GAP-Auth" Username
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "trees"
         :> Raw
    :<|> Header "GAP-Auth" Username
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "blobs"
         :> Raw
    :<|> Header "GAP-Auth" Username
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
-- incorporate into a larger JSON response. This should probably some
-- better RPC mechanism.
poorMansJsonGet :: (FromJSON a) => Manager -> Text -> IO (Maybe a)
poorMansJsonGet manager endpoint = do
    r <- parseUrl (T.unpack endpoint)
    let rJson = r { requestHeaders = [(hAccept, "application/json")] }
    resp <- withResponse rJson manager $ \response -> do
      body <- fmap fromStrict (responseBody response)
      return $ decode' body
    pure resp


browse :: AppConf -> Maybe Username -> Owner -> Repo -> ExceptT (APIError BrowseError) IO BrowseMetaResponse
browse AppConf{httpManager, repoHostname, repoPort} _maybeUsername owner repo = do
    r <- liftIO $ poorMansJsonGet httpManager ("http://" <> repoHostname <> ":" <> fromShow repoPort <> "/v1/repos/" <> owner <> "/" <> repo)
    repoMeta <- case r of
        Just x -> pure x
        Nothing -> throwE (SubAPIError NotFound)
    return BrowseMetaResponse
      { _BrowseMetaResponse_repo_meta = repoMeta
      , _BrowseMetaResponse_description = "fake description"
      , _BrowseMetaResponse_created_at = Time.LocalTime (Time.ModifiedJulianDay 2000) (Time.TimeOfDay 1 1 1)
      }

-- Tree, commit & blob are passed straight through if they meet the
-- authentication requirements.
treeCommitBlob :: AppConf -> Maybe Username -> Owner -> Repo -> Application
treeCommitBlob AppConf{httpManager, repoHostname, repoPort} _maybeUsername owner _repo =
    waiProxyTo proxy defaultOnExc httpManager
  where
    -- pass the raw request through if the user is authorized
    proxy request =
        return $ case owner of
            "jml" -> WPRResponse (responseLBS status404 [] "not found")
            _  -> WPRModifiedRequest request (ProxyDest (encodeUtf8 repoHostname) repoPort)
