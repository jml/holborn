{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Env
import Servant
  ( serve
  , Get
  , Proxy(..)
  , Server
  )
import Holborn.Api.Internal (AuthAPI, server)

rootAPI :: Proxy AuthAPI
rootAPI = Proxy

app :: Application
app = serve rootAPI server


main :: IO ()
main = do
    port <- Env.parse (Env.header "Run holborn API server") $
            Env.var (Env.auto Env.<=< Env.nonempty) "PORT" (Env.help "port to listen on")
    run port app
