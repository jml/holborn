{-| Web frontend to syntax analyzer.

Intended to provide:
  1. An API for other, user-facing frontends to call out to.
  2. A development server for iterating on syntax highlighting.

-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.Web
       ( RootAPI
       , rootAPI
       , server
       ) where

import BasicPrelude

import Servant
  ( Get
  , Proxy(..)
  , Server
  )
import Servant.HTML.Blaze

import Text.Blaze (ToMarkup)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Get the typeclass instances for converting Holborn stuff to HTML.
import Holborn.HtmlFormat ()
import Holborn.Scope (ID)
import Holborn.Style (monokai)
import Holborn.Syntax (annotatePythonCode)
import Holborn.Types (AnnotatedSource)


type RootAPI = Get '[HTML] (AnnotatedSource ID)


rootAPI :: Proxy RootAPI
rootAPI = Proxy


server :: Text -> Server RootAPI
server pythonSourceCode = return (annotatePythonCode pythonSourceCode)


-- | Given a rendered HTML block of code, return a full HTML with highlighting.
codePage :: ToMarkup a => a -> Html
codePage codeHtml = do
  H.head $ do
    H.title "Example code page"
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml monokai
    -- XXX: Embedding styling here is terrible.
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml (".codehilite { background-color: #333; }" :: Text)
  H.body $ H.div ! A.class_ (H.toValue ("codehilite" :: Text)) $ toHtml codeHtml
