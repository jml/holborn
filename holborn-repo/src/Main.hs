{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import HolbornPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings)
import Servant (serve)
import qualified Env
import Control.Concurrent (forkIO)

import Holborn.Repo.HttpProtocol (repoServer, repoAPI)
import Holborn.Repo.Config (Config(..), warpSettings)
import Holborn.Repo.RawProtocol (serveRaw)
import qualified Network.Wai.Middleware.RequestLogger as RL


-- git init --bare /tmp/hello
app :: Config -> Application
app config = serve repoAPI (repoServer config)

-- | Load configuration from the environment.
loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "run a holborn repo server") $
  Config <$> Env.var (Env.str Env.<=< Env.nonempty) "REPO_ROOT" (Env.help "path to root of ./<repoId> bare repositories")
         <*> Env.var Env.auto "PORT" (Env.def 8080 <> Env.help "Port to listen on")
         <*> Env.var Env.auto "RAW_PORT" (Env.def 8081 <> Env.help "Port to listen on for raw connections")


main :: IO ()
main = do
    config <- loadConfig
    void $ forkIO (serveRaw config)
    runSettings (warpSettings config) (RL.logStdoutDev (app config))
