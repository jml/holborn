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

import qualified Holborn.API.Api as AApi
import qualified Holborn.API.Internal as AInternal
import qualified Holborn.API.Settings.SSHKeys as ASSHKeys
import qualified Holborn.Docs as Docs
import Holborn.API.Types (AppConf(..))
import Servant ((:<|>)(..))
import Data.Proxy (Proxy(..))


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


api :: Proxy (AApi.API :<|> AInternal.API :<|> ASSHKeys.API :<|> Docs.API)
api = Proxy


app :: AppConf -> Application
app conf = serve api
  ((AApi.server conf) :<|> AInternal.server :<|> (ASSHKeys.server conf) :<|> Docs.server)



main = do
    config <- loadConfig
    conn <- connect (defaultConnectInfo  { connectDatabase = "holborn", connectUser = "tom"})
    let conf = AppConf conn "test-secret-todo-read-from-env"
    Warp.runSettings (warpSettings config) (app conf)
