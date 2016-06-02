{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.API
  ( FullAPI
  , server
  , api
  ) where

import Servant (Server, (:<|>)(..), (:>))
import Data.Proxy (Proxy(..))

import qualified Holborn.Docs
import qualified Holborn.API.SSH
import qualified Holborn.API.Browse
import qualified Holborn.API.NewRepo
import Holborn.API.Config (AppConf)
import qualified Holborn.API.Settings.SSHKeys
import qualified Holborn.API.Settings.Profile


type FullAPI =
         "internal" :> Holborn.API.SSH.API
    :<|> "docs" :> Holborn.Docs.API
    :<|> "v1" :> ( Holborn.API.Settings.SSHKeys.API
                   :<|> Holborn.API.Settings.Profile.API
                   :<|> Holborn.API.NewRepo.API
                 )
    :<|> "v1" :> "repos" :> Holborn.API.Browse.API


api :: Proxy FullAPI
api = Proxy


server :: AppConf -> Server FullAPI
server conf = Holborn.API.SSH.server conf
              :<|> Holborn.Docs.server
              :<|> (Holborn.API.Settings.SSHKeys.server conf
                    :<|> Holborn.API.Settings.Profile.server conf
                    :<|> Holborn.API.NewRepo.server conf
                   )
              :<|> Holborn.API.Browse.server conf
