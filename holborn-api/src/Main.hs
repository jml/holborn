{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
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
main = run 8082 app
