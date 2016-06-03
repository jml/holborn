{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import BasicPrelude

import qualified Env
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleCorsResourcePolicy, simpleHeaders)

import Holborn.API (api, server)
import Holborn.API.Config (AppConf, Config(..), loadAppConf)
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
      "HOLBORN_REPO_HOSTNAME" (Env.def "127.0.0.1" <> Env.help "Where the holborn-repo server is running. e.g. 127.0.0.1")
  <*> Env.var Env.auto
      "HOLBORN_REPO_PORT" (Env.def 8080 <> Env.help "What port the holborn-repo server is listening on. e.g. 8080")
  <*> Env.var (Env.str Env.<=< Env.nonempty)
      "HOLBORN_REPO_RAW_HOSTNAME" (Env.def "127.0.0.1" <> Env.help "Where the holborn-repo raw server is running. e.g. 127.0.0.1")
  <*> Env.var Env.auto
      "HOLBORN_REPO_RAW_PORT" (Env.def 8081 <> Env.help "What port the holborn-repo raw server is listening on. e.g. 8081")


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


devCors :: a -> Maybe CorsResourcePolicy
devCors _ = Just (simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders ++ ["Authorization"] })


app :: AppConf -> Application
app conf = serve api (server conf)


main :: IO ()
main = do
    conf@Config{..} <- loadConfig
    print ("Using config:" :: Text)
    print conf
    appConf <- loadAppConf conf
    Warp.runSettings (warpSettings port) (cors devCors (app appConf))
