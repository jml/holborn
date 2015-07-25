module Holborn.Web
       ( HolbornToken
       , codePage
       , renderPythonCode
       ) where

import BasicPrelude

import qualified Data.List as List
import Data.Maybe (fromJust)

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Highlighter.Lexer (runLexer)
import Text.Highlighter.Lexers (lexers)
import Text.Highlighter.Types (Token)

import Holborn.HtmlFormat (format)
import Holborn.Style (monokai)


-- | A token with extra semantic information. More data to be added later.
data HolbornToken = HolbornToken Token


-- | Get a token that can be fed into our highlighting library.
getHighlightingToken :: HolbornToken -> Token
getHighlightingToken (HolbornToken t) = t


-- | Placeholder data structure for AST
data AST = AST


annotateTokens :: [Token] -> AST -> [HolbornToken]
annotateTokens tokens _ = map HolbornToken tokens


formatHtmlBlock :: [HolbornToken] -> Html
formatHtmlBlock tokens =
  format includeLineNumbers (map getHighlightingToken tokens)
  where includeLineNumbers = False


lexPythonCode :: Text -> [Token]
lexPythonCode code =
  fromRight $ runLexer pythonLexer (encodeUtf8 code)
  where pythonLexer = fromJust $ List.lookup ".py" lexers


fromRight :: Show a => Either a b -> b
fromRight (Left e) = terror $ show e
fromRight (Right r) = r


-- XXX: When the pattern matching here fails, we get
-- src/Main.hs:21:5-52: Irrefutable pattern failed for pattern (Just pythonLexer)
-- and an empty response, rather than the expected 500

-- XXX: Actually handle cases where code won't lex or we can't find a lexer

-- XXX: Is Text the right type?


-- | Take some Python code and turn it into HTML
renderPythonCode :: Text -> Html
renderPythonCode pythonCode =
  let simpleTokens = lexPythonCode pythonCode
      ast = undefined
      annotatedTokens = annotateTokens simpleTokens ast
  in
    formatHtmlBlock annotatedTokens


-- | Given a rendered HTML block of code and return a full HTML with
-- highlighting.
codePage :: Html -> Html
codePage codeHtml = do
  H.head $ do
    H.title "Example code page"
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml monokai
    -- XXX: Embedding styling here is terrible.
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml (".codehilite { background-color: #333; }" :: Text)
  H.body $ H.div ! A.class_ (H.toValue ("codehilite" :: Text)) $ codeHtml
