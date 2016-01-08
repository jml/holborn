{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve)
import Database.PostgreSQL.Simple (connect, ConnectInfo(..), defaultConnectInfo)

import Holborn.UI.Api (userAPI, server)
import Holborn.UI.Types (AppConf(..))

app :: AppConf -> Application
app conf = serve userAPI (server conf)


main = do
    conn <- connect (defaultConnectInfo  { connectDatabase = "holborn", connectUser = "tom"})
    let conf = AppConf conn "test-secret-todo-read-from-env"
    Warp.run 8002 (app conf)
