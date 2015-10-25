{-| Web frontend to syntax analyzer.

Intended to provide:
  1. An API for other, user-facing frontends to call out to.
  2. A development server for iterating on syntax highlighting.

-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.Web
       ( SyntaxAPI
       , syntaxAPI
       , server
       ) where

import BasicPrelude

import Control.Monad.Trans.Either (EitherT)
import Servant
import Servant.HTML.Blaze
import System.FilePath (joinPath)

import Text.Blaze (ToMarkup)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Get the typeclass instances for converting Holborn stuff to HTML.
import Holborn.HtmlFormat ()
import Holborn.Style (monokai)
import Holborn.Syntax ( HolbornSource
                      , annotatePythonCode
                      )


-- QUESTION: Can we compose configuration structures so that the
-- server-specific stuff (like PORT) is separate from the application-specific
-- stuff (like path to code)? Is it even worth it?

-- QUESTION: Should we have a Config object (and presumably ReaderT monad), or
-- should we just pass parameters around as needed?

-- TODO: We can probably get rid of 'demo' real soon now.
type SyntaxAPI =
       "demo"  :> Get '[HTML] HolbornSource
  :<|> "files" :> PathAPI


syntaxAPI :: Proxy SyntaxAPI
syntaxAPI = Proxy


-- TODO: Is there a better type for this (e.g. one that ensures no slashes,
-- regular character set).
type PathSegment = Text

-- TODO: Crappy, temporary API. We actually want to allow for multiple path
-- segments, but that requires some mucking around with Servant which is more
-- than we want to do right now.
type PathAPI =
       Capture "single" PathSegment :> Get '[HTML] HolbornSource
  :<|> Capture "double1" PathSegment :> Capture "double2" PathSegment :> Get '[HTML] HolbornSource
  :<|> Capture "triple1" PathSegment :> Capture "triple2" PathSegment :> Capture "triple3" PathSegment :> Get '[HTML] HolbornSource

type PathHandler = EitherT ServantErr IO


-- | Given a base path and a list of path segments, render the code found on
-- disk at that path.
renderCode :: FilePath -> [PathSegment] -> PathHandler HolbornSource
renderCode base segments = renderCode' $ base </> joinPath (map textToString segments)


-- | Given a path to a file on disk, render the code.
renderCode' :: FilePath -> PathHandler HolbornSource
renderCode' path = do
  sourceCode <- liftIO $ readFile path  -- TODO: Handle error.
  return $ annotatePythonCode sourceCode


browseCode :: FilePath -> Server PathAPI
browseCode basePath =
       (\x     -> renderCode basePath [x])
  :<|> (\x y   -> renderCode basePath [x, y])
  :<|> (\x y z -> renderCode basePath [x, y, z])


-- | Create a server for SyntaxAPI
server :: Text -> FilePath -> Server SyntaxAPI
server pythonSourceCode basePath = return (annotatePythonCode pythonSourceCode) :<|> browseCode basePath


-- TODO: Either use this or kill it with fire.
-- | Given a rendered HTML block of code, return a full HTML with highlighting.
codePage :: ToMarkup a => a -> Html
codePage codeHtml = do
  H.head $ do
    H.title "Example code page"
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml monokai
    -- XXX: Embedding styling here is terrible.
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml (".codehilite { background-color: #333; }" :: Text)
  H.body $ H.div ! A.class_ (H.toValue ("codehilite" :: Text)) $ toHtml codeHtml
