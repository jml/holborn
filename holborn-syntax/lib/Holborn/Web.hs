module Holborn.Web
       ( codePage
       ) where

import BasicPrelude

import Text.Blaze (ToMarkup)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Holborn.Style (monokai)


-- | Given a rendered HTML block of code and return a full HTML with
-- highlighting.
codePage :: ToMarkup a => a -> Html
codePage codeHtml = do
  H.head $ do
    H.title "Example code page"
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml monokai
    -- XXX: Embedding styling here is terrible.
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml (".codehilite { background-color: #333; }" :: Text)
  H.body $ H.div ! A.class_ (H.toValue ("codehilite" :: Text)) $ toHtml codeHtml
