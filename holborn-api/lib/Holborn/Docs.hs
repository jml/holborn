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
import qualified Holborn.API.Keys as AKeys
import qualified Holborn.JSON.Keys
import qualified Holborn.API.Types


type API =
    "docs" :> Get '[PlainText] Text


docsKeys :: Servant.Docs.API
docsKeys = docsWithIntros [] (Proxy :: Proxy AKeys.API)


server :: Server API
server = return (pack (markdown docsKeys))


instance ToSample [Holborn.JSON.Keys.ListKeysRow] Text where
    toSample _ = Just "many"


instance ToSample Holborn.JSON.Keys.ListKeysRow Text where
    toSample _ = Just "Hello, haskeller!"


instance ToSample () Text where
    toSample _ = Just "unit!"


instance ToSample Holborn.JSON.Keys.AddKeyData Text where
    toSample _ = Just "unit!"


instance ToCapture (Capture "id" Int) where
    toCapture _ = DocCapture "capture id" "capture id long"


instance ToCapture (Capture "username" Holborn.API.Types.Username) where
    toCapture _ = DocCapture "capture id" "capture id long"
