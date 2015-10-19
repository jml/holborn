{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve, Server, Proxy)
import qualified Env

import Holborn.Repo.HttpProtocol (repoServer, repoAPI, Config(..))

-- git init --bare /tmp/hello
app :: Config -> Application
app config = serve repoAPI (repoServer config)

main :: IO ()
main = do
    config <- Env.parse (Env.header "run a holborn repo server") $
        Config <$> Env.var (Env.str Env.<=< Env.nonempty) "REPO" (Env.help "path to the repos git-dir")
    run 8080 (app config)
