{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve)

import Holborn.UI.Api (userAPI, server)

app :: Application
app = serve userAPI server


main = do
    Warp.run 8002 app
