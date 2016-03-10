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

import Data.Aeson (Value(..), object, (.=))
import Servant

import Holborn.API.Types (AppConf(..), Username)
import Holborn.JSON.Browse as B
import Holborn.Auth (AuthToken(..), Permission(..), hasPermission, getAuthFromToken)
import Holborn.Errors (JSONCodeableError(..))
import Network.Wai (Application, requestMethod, rawPathInfo)
import Network.HTTP.ReverseProxy (waiProxyTo, defaultOnExc, WaiProxyResponse(WPRModifiedRequest), ProxyDest(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)


type API =
    "v1" :> Header "Authorization" AuthToken :> "browse" :> ReqBody '[JSON] B.BrowseRequest :> Raw

data BrowseError = NotFound

instance JSONCodeableError BrowseError where
    toJSON NotFound = (404, object ["message" .= ("could not find object" :: Text)])


server :: AppConf -> Server API
server conf = browse conf


browse :: AppConf -> Maybe AuthToken -> B.BrowseRequest -> Application
browse AppConf{conn, httpManager} token B.BrowseRequest{..} =
    waiProxyTo proxy defaultOnExc httpManager
  where
    -- This is a hack to map repositories to the current REPO
    -- backend. Eventually we'll just clean & then pass through the
    -- API request (can't trust internet inputs).
    proxy request = do
        return $ let modifiedRequest = request { requestMethod = "GET", rawPathInfo = encodeUtf8 _BrowseRequest_repo }
                 in  WPRModifiedRequest modifiedRequest (ProxyDest "127.0.0.1" 8080)
