module Holborn.API.Templates (landingPage) where

import BasicPrelude
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


bootstrapUrl :: Text
bootstrapUrl = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"


-- | Generate the landing page for Holborn.
landingPage :: Text -> Text -> H.Html
landingPage baseUrl staticBaseUrl = H.docTypeHtml $ do
  H.head $ do
    H.title "norf"
    H.link ! A.href (H.toValue bootstrapUrl) ! A.rel "stylesheet" ! A.type_ "text/css"
  H.body $ do
    H.div ! A.id "container" ! A.class_ "container" $ do
      H.script $ H.toMarkup $ "window.holbornBaseUrl = \"" ++ baseUrl ++ "\";"
      H.script ! A.src reactPath $ ""
      H.script ! A.src appJsPath $ ""

  where
    reactPath = H.toValue $ staticBaseUrl ++ "/bower_components/react/react.js"
    appJsPath = H.toValue $ staticBaseUrl ++ "/app.js"

