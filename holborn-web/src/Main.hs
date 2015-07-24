module Main where

import BasicPrelude

import qualified Data.List as List
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Highlighter.Formatters.Html (format)
import Text.Highlighter.Lexer (runLexer)
import Text.Highlighter.Lexers (lexers)
import Web.Spock.Safe

import ExampleData (examplePython)


-- XXX: When the pattern matching here fails, we get
-- src/Main.hs:21:5-52: Irrefutable pattern failed for pattern (Just pythonLexer)
-- and an empty response, rather than the expected 500

-- XXX: Actually handle cases where code won't lex or we can't find a lexer

-- XXX: Is Text the right type?

renderPythonCode :: Text -> Html
renderPythonCode pythonCode =
  -- True means "include line numbers".
  format True tokens
  where
    (Right tokens) = runLexer pythonLexer (encodeUtf8 pythonCode)
    (Just pythonLexer) = List.lookup ".py" lexers


-- XXX: Is there a better way of sending blaze to the user with Spock?
blaze :: MonadIO m => Html -> ActionT m a
blaze = lazyBytes . renderHtml


main :: IO ()
main =
    runSpock 8080 $ spockT id $
    do get root $
           blaze $ renderPythonCode examplePython
       get ("hello" <//> var) $ \name ->
           text ("Hello " <> name <> "!")
