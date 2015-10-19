{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve, Server, Proxy)

import Holborn.Repo.HttpProtocol (repoServer, repoAPI, Config(..))

-- git init --bare /tmp/hello
app :: Application
app = serve repoAPI (repoServer (Config "/tmp/hello"))

main :: IO ()
main = run 8080 app
