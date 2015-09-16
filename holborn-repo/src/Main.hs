{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve, Server, Proxy)

import Holborn.Repo.HttpProtocol (repoServer, repoAPI)

app :: Application
app = serve repoAPI repoServer

main :: IO ()
main = run 8080 app
