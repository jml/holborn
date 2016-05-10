{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import qualified Env
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve, (:<|>)(..))
import Data.Proxy (Proxy(..))
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleCorsResourcePolicy, simpleHeaders)

import qualified Holborn.Docs
import qualified Holborn.API.Api
import qualified Holborn.API.Internal
import qualified Holborn.API.Browse
import Holborn.API.Config (AppConf, Config(..), loadAppConf)
import qualified Holborn.API.Settings.SSHKeys
import qualified Holborn.API.Settings.Profile
import qualified Holborn.Logging as Log


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
  <*> Env.var (Env.auto Env.<=< Env.nonempty)
      "HOLBORN_PG_PORT" (Env.def 5432 <> Env.help "pg port")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_BASE_URL" (Env.def "http://127.0.0.1:8002" <> Env.help "e.g. http://127.0.0.1:8002")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_STATIC_BASE_URL" (Env.def "http://127.0.0.1:1337" <> Env.help "e.g. http://127.0.0.1:1337")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_REPO_HOSTNAME" (Env.help "Where the holborn-repo server is running. e.g. 127.0.0.1")
  <*> Env.var Env.auto
      "HOLBORN_REPO_PORT" (Env.help "What port the holborn-repo server is listening on. e.g. 8080")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_REPO_RAW_HOSTNAME" (Env.help "Where the holborn-repo raw server is running. e.g. 127.0.0.1")
  <*> Env.var Env.auto
      "HOLBORN_REPO_RAW_PORT" (Env.help "What port the holborn-repo raw server is listening on. e.g. 8081")


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


devCors _ = Just (simpleCorsResourcePolicy { corsRequestHeaders = (simpleHeaders ++ ["Authorization"]) })


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


main :: IO ()
main = do
    conf@Config{..} <- loadConfig
    print "Using config:"
    print conf
    appConf <- loadAppConf conf
    Warp.runSettings (warpSettings port) (cors devCors (app appConf))
