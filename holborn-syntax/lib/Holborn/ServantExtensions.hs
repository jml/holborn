{-| Extensions to Servant

We should keep this module as small as possible, submitting things upstream as
much as we can.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.ServantExtensions (CaptureAll) where

import BasicPrelude
import GHC.TypeLits (KnownSymbol, Symbol)
import Network.Wai (pathInfo)
import Servant
import Servant.Server.Internal (processedPathInfo)


data CaptureAll (sym :: Symbol) deriving (Typeable)

instance (KnownSymbol capture, HasServer sublayout)
         => HasServer (CaptureAll capture :> sublayout) where

  type ServerT (CaptureAll capture :> sublayout) m = [Text] -> ServerT sublayout m

  route Proxy subserver request respond =
    let request' = request { pathInfo = [] }
        segments = processedPathInfo request
    in
    route (Proxy :: Proxy sublayout) (subserver segments) request' respond
