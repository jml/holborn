module Main where

import BasicPrelude

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Highlighting.Kate (
  defaultFormatOpts,
  formatHtmlBlock,
  highlightAs,
  styleToCss,
  tango,  -- XXX: Just copied from the example, not necessarily what we want
  )

import Web.Spock.Safe

import ExampleData (examplePython)


-- XXX: When the pattern matching here fails, we get
-- src/Main.hs:21:5-52: Irrefutable pattern failed for pattern (Just pythonLexer)
-- and an empty response, rather than the expected 500

-- XXX: Actually handle cases where code won't lex or we can't find a lexer

-- XXX: Is Text the right type?


-- | Take some Python code and turn it into HTML
renderPythonCode :: Text -> Html
renderPythonCode = formatHtmlBlock defaultFormatOpts . highlightAs "python" . textToString


-- | Given a rendered HTML block of code and return a full HTML with
-- highlighting.
codePage :: Html -> Html
codePage codeHtml = do
  H.head $ H.style ! A.type_ (H.toValue ("text/css" :: Text))
    $ toHtml $ styleToCss tango
  H.body codeHtml


-- XXX: Is there a better way of sending blaze to the user with Spock?
blaze :: MonadIO m => Html -> ActionT m a
blaze = lazyBytes . renderHtml


main :: IO ()
main =
    runSpock 8080 $ spockT id $
    get root $ blaze $ codePage $ renderPythonCode examplePython
