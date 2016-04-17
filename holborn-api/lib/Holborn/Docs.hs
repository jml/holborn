{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Holborn.Docs
       ( API
       , server
       ) where

import BasicPrelude

import Servant.Docs (docsWithIntros, markdown, noSamples, ToSample(..), ToCapture(..), DocCapture(..))
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


instance ToSample Holborn.JSON.Settings.SSHKeys.ListKeysRow where
    toSamples _ = noSamples


instance ToSample Holborn.JSON.Settings.SSHKeys.AddKeyData where
    toSamples _ = noSamples


instance ToCapture (Capture "id" Int) where
    toCapture _ = DocCapture "capture id" "capture id long"


instance ToCapture (Capture "username" Holborn.API.Types.Username) where
    toCapture _ = DocCapture "capture id" "capture id long"
