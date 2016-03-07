{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import qualified Env
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve)
import Database.PostgreSQL.Simple (connect, ConnectInfo(..), defaultConnectInfo)

import Holborn.API.Types (AppConf(..))
import Servant ((:<|>)(..))
import Data.Proxy (Proxy(..))

import qualified Holborn.Docs
import qualified Holborn.API.Api
import qualified Holborn.API.Internal
import qualified Holborn.API.Browse
import qualified Holborn.API.Settings.SSHKeys
import qualified Holborn.API.Settings.Profile
import Network.HTTP.Client (newManager, defaultManagerSettings)


data Config = Config { _port :: Warp.Port
                     }


loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "server") $
  Config <$> Env.var Env.auto "PORT"       (Env.def 8002 <> Env.help "Port to listen on")


-- XXX: Duplicated & modified from Holborn.Repo.Config
-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Warp.Settings
warpSettings config =
  Warp.setBeforeMainLoop printPort (Warp.setPort port' Warp.defaultSettings)
  where
    printPort = putStrLn $ "holborn-api running at http://localhost:" ++ show port' ++ "/"
    port' = _port config


type FullAPI =
    Holborn.API.Api.API
    :<|> Holborn.API.Internal.API
    :<|> Holborn.API.Settings.SSHKeys.API
    :<|> Holborn.API.Settings.Profile.API
    :<|> Holborn.API.Browse.API
    :<|> Holborn.Docs.API


api :: Proxy FullAPI
api = Proxy

app :: AppConf -> Application
app conf = serve api $
  Holborn.API.Api.server conf
   :<|> Holborn.API.Internal.server conf
   :<|> Holborn.API.Settings.SSHKeys.server conf
   :<|> Holborn.API.Settings.Profile.server conf
   :<|> Holborn.API.Browse.server conf
   :<|> Holborn.Docs.server


main = do
    config <- loadConfig
    conn <- connect (defaultConnectInfo  { connectDatabase = "holborn", connectUser = "tom"})
    httpManager <- newManager defaultManagerSettings
    let conf = AppConf conn "test-secret-todo-read-from-env" httpManager
    Warp.runSettings (warpSettings config) (app conf)
