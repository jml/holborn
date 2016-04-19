{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Holborn.ServantTypes (RenderedJson) where


import Servant (Accept(..))
import qualified Network.HTTP.Media as M
import Data.Aeson (encode, ToJSON)

data RenderedJson

-- We make up a fake content type "application/r-json" which is json
-- encoded content but rendered for human consumption (e.g. syntax
-- highlighted, linking to definitions) where applicable.
instance Accept RenderedJson where
    contentType _ = "application" M.// "r-json"
