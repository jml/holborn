{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Holborn.Docs
       ( API
       , server
       ) where

import BasicPrelude

import Servant.Docs (docsWithIntros, markdown, ToSample(..), ToCapture(..), DocCapture(..))
import qualified Servant.Docs
import Servant

import Data.Text (pack)
import qualified Holborn.API.Settings.SSHKeys
import qualified Holborn.JSON.Settings.SSHKeys
import qualified Holborn.API.Types


type API =
    "docs" :> Get '[PlainText] Text


docsKeys :: Servant.Docs.API
docsKeys = docsWithIntros [] (Proxy :: Proxy Holborn.API.Settings.SSHKeys.API)


server :: Server API
server = return (pack (markdown docsKeys))


instance ToSample [Holborn.JSON.Settings.SSHKeys.ListKeysRow] Text where
    toSample _ = Just "many"


instance ToSample Holborn.JSON.Settings.SSHKeys.ListKeysRow Text where
    toSample _ = Just "Hello, haskeller!"


instance ToSample () Text where
    toSample _ = Just "unit!"


instance ToSample Holborn.JSON.Settings.SSHKeys.AddKeyData Text where
    toSample _ = Just "unit!"


instance ToCapture (Capture "id" Int) where
    toCapture _ = DocCapture "capture id" "capture id long"


instance ToCapture (Capture "username" Holborn.API.Types.Username) where
    toCapture _ = DocCapture "capture id" "capture id long"
