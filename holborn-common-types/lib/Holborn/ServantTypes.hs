{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Holborn.ServantTypes (RenderedJson) where

import Servant (Accept(..), MimeRender(..))
import qualified Network.HTTP.Media as M
import Data.Aeson (Value, encode)

data RenderedJson

-- We make up a fake content type "application/r-json" which is json
-- encoded content but rendered for human consumption (e.g. syntax
-- highlighted, linking to definitions) where applicable.
instance Accept RenderedJson where
    contentType _ = "application" M.// "r-json"


instance MimeRender RenderedJson Value where
  mimeRender _ = encode
