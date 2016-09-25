{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}

module Holborn.API
  ( app
  ) where

import Data.Proxy (Proxy(..))
import Network.Wai (Application)
import Servant (Server, (:<|>)(..), (:>), serve)

import qualified Holborn.API.SSH
import qualified Holborn.API.Browse
import qualified Holborn.API.NewRepo
import qualified Holborn.API.CreateAccount
import Holborn.API.Config (Config)
import qualified Holborn.API.Settings.SSHKeys
import qualified Holborn.API.Settings.Profile


type API =
         "internal" :> Holborn.API.SSH.API
    :<|> "v1" :> ( Holborn.API.Settings.SSHKeys.API
                   :<|> Holborn.API.Settings.Profile.API
                   :<|> Holborn.API.NewRepo.API
                   :<|> Holborn.API.CreateAccount.API
                 )
    :<|> "v1" :> "repos" :> Holborn.API.Browse.API


api :: Proxy API
api = Proxy


server :: Config -> Server API
server conf = Holborn.API.SSH.server conf
              :<|> (Holborn.API.Settings.SSHKeys.server conf
                    :<|> Holborn.API.Settings.Profile.server conf
                    :<|> Holborn.API.NewRepo.server conf
                    :<|> Holborn.API.CreateAccount.server conf
                   )
              :<|> Holborn.API.Browse.server conf


-- | Create a WAI application for Holborn
app :: Config -> Application
app conf = serve api (server conf)
