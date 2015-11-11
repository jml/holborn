{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings)
import Servant (serve)
import qualified Env
import Control.Concurrent (forkIO)

import Holborn.Repo.HttpProtocol (repoServer, repoAPI)
import Holborn.Repo.Config (Config(..), warpSettings)
import Holborn.Repo.RawProtocol (serveRaw)

-- git init --bare /tmp/hello
app :: Config -> Application
app config = serve repoAPI (repoServer config)

-- | Load configuration from the environment.
loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "run a holborn repo server") $
  Config <$> Env.var (Env.str Env.<=< Env.nonempty) "REPO_ROOT" (Env.help "path to root of ./org/repo bare repositories")
             -- XXX: "PORT" copied from holborn-syntax Main.
         <*> Env.var Env.auto "PORT" (Env.def 8080 <> Env.help "Port to listen on")


main :: IO ()
main = do
    config <- loadConfig
    void $ forkIO (serveRaw config)
    runSettings (warpSettings config) (app config)
