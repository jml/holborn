{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import qualified Env
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Database.PostgreSQL.Simple (connect, ConnectInfo(..), defaultConnectInfo)

import Holborn.API.Types (AppConf(..))
import Servant (serve, (:<|>)(..))
import Data.Proxy (Proxy(..))

import qualified Holborn.Docs
import qualified Holborn.API.Api
import qualified Holborn.API.Internal
import qualified Holborn.API.Browse
import qualified Holborn.API.Settings.SSHKeys
import qualified Holborn.API.Settings.Profile
import qualified Holborn.Logging as Log
import Network.HTTP.Client (newManager, defaultManagerSettings)


data Config = Config { _port :: Warp.Port
                     , pgDb :: String
                     , pgUser :: String
                     , baseUrl :: String
                     , staticBaseUrl :: String
                     }


loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "server") $
  Config
  <$> Env.var Env.auto
      "PORT" (Env.def 8002 <> Env.help "Port to listen on")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_PG_DATABASE" (Env.def "holborn" <> Env.help "pg database name")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_PG_USER" (Env.def "holborn" <> Env.help "pg user")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_BASE_URL" (Env.def "http://127.0.0.1:8002" <> Env.help "e.g. http://127.0.0.1:8002")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_STATIC_BASE_URL" (Env.def "http://127.0.0.1:1337" <> Env.help "e.g. http://127.0.0.1:1337")


-- XXX: Duplicated & modified from Holborn.Repo.Config
-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Warp.Port -> Warp.Settings
warpSettings port =
  Warp.setBeforeMainLoop printPort (Warp.setPort port' Warp.defaultSettings)
  where
    printPort = Log.info $ "holborn-api running at http://localhost:" ++ show port' ++ "/"
    port' = port


type FullAPI =
         Holborn.API.Internal.API
    :<|> Holborn.API.Settings.SSHKeys.API
    :<|> Holborn.API.Settings.Profile.API
    :<|> Holborn.API.Browse.API
    :<|> Holborn.Docs.API
    :<|> Holborn.API.Api.API


api :: Proxy FullAPI
api = Proxy

app :: AppConf -> Application
app conf = serve api $
        Holborn.API.Internal.server conf
   :<|> Holborn.API.Settings.SSHKeys.server conf
   :<|> Holborn.API.Settings.Profile.server conf
   :<|> Holborn.API.Browse.server conf
   :<|> Holborn.Docs.server

   -- NB that Api.server has a catch-all at the end so we can catch
   -- routes like /settings/ssh-keys that only have a meaning client
   -- side for now. As a consequence `Holborn.API.Api.server` MUST BE
   -- LAST in the :<|> combinator.
   :<|> Holborn.API.Api.server conf


main = do
    Config{..} <- loadConfig
    conn <- connect (defaultConnectInfo  { connectDatabase = pgDb, connectUser = pgUser})
    httpManager <- newManager defaultManagerSettings
    let conf = AppConf conn "test-secret-todo-read-from-env" httpManager (fromString baseUrl) (fromString staticBaseUrl)
    Warp.runSettings (warpSettings _port) (app conf)
