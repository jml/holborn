{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import qualified Env
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve)
import Database.PostgreSQL.Simple (connect, ConnectInfo(..), defaultConnectInfo)

import qualified Holborn.API.Api as AA
import qualified Holborn.API.Internal as AI
import Holborn.API.Types (AppConf(..))
import Servant ((:<|>)(..))
import Data.Proxy (Proxy(..))


data Config = Config { _port :: Warp.Port
                     }

loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "server") $
  Config <$> Env.var Env.auto "PORT"       (Env.def 8002 <> Env.help "Port to listen on")


api :: Proxy (AA.UserAPI :<|> AI.AuthAPI)
api = Proxy


app :: AppConf -> Application
app conf = serve api
  ((AA.server conf) :<|> AI.server)


main = do
    config <- loadConfig
    conn <- connect (defaultConnectInfo  { connectDatabase = "holborn", connectUser = "tom"})
    let conf = AppConf conn "test-secret-todo-read-from-env"
    Warp.run (_port config) (app conf)
