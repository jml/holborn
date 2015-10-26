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
import Holborn.Api.Internal (API, server)

rootAPI :: Proxy API
rootAPI = Proxy

app :: Application
app = serve rootAPI server

main :: IO ()
main = run 8082 app
