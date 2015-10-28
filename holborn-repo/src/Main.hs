{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import qualified Env
import Control.Concurrent (forkIO)

import Holborn.Repo.HttpProtocol (repoServer, repoAPI)
import Holborn.Repo.Config (Config(..))
import Holborn.Repo.RawProtocol (serveRaw)

-- git init --bare /tmp/hello
app :: Config -> Application
app config = serve repoAPI (repoServer config)

main :: IO ()
main = do
    config <- Env.parse (Env.header "run a holborn repo server") $
        Config <$> Env.var (Env.str Env.<=< Env.nonempty) "REPO_ROOT" (Env.help "path to root of ./org/repo bare repositories")
    _ <- forkIO (serveRaw config)
    run 8080 (app config)
