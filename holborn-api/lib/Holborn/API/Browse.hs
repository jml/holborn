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
import Holborn.API.Auth (getAuthFromToken)
import Holborn.Auth (AuthToken(..), Permission(..), hasPermission)
import Holborn.Errors (JSONCodeableError(..))
import Network.Wai (Application, responseLBS)
import Network.HTTP.ReverseProxy (waiProxyTo, defaultOnExc, WaiProxyResponse(WPRModifiedRequest, WPRResponse), ProxyDest(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (status404)

type Owner = Text
type Repo = Text

type API =
    "v1"
    :> "repos"
    :> Header "Authorization" AuthToken
    :> Capture "owner" Owner
    :> Capture "repo" Repo
    :> Raw


data BrowseError = NotFound

instance JSONCodeableError BrowseError where
    toJSON NotFound = (404, object ["message" .= ("could not find object" :: Text)])


server :: AppConf -> Server API
server conf = browse conf


browse :: AppConf -> Maybe AuthToken -> Owner -> Repo -> Application
browse AppConf{conn, httpManager} token owner repo =
    waiProxyTo proxy defaultOnExc httpManager
  where
    -- pass the raw request through if the user is authorized
    proxy request = do
        return $ case owner of
            "jml" -> WPRResponse (responseLBS status404 [] "not found")
            _  -> WPRModifiedRequest request (ProxyDest "127.0.0.1" 8080)
