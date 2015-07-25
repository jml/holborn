module Holborn.Web
       ( HolbornToken
       , codePage
       , renderPythonCode
       , annotateTokens
       ) where

import BasicPrelude

import qualified Data.List as List
import Data.Maybe (fromJust)

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Highlighter.Formatters.Html (format)
import Text.Highlighter.Lexer (runLexer)
import Text.Highlighter.Lexers (lexers)
import Text.Highlighter.Types (Token(tText))

import Holborn.Style (monokai)


-- | A token with extra semantic information. More data to be added later.
data Reference = Reference Text
data HolbornToken = HolbornToken Token Reference


-- | Get a token that can be fed into our highlighting library.
getHighlightingToken :: HolbornToken -> Token
getHighlightingToken (HolbornToken t _) = t


-- | Placeholder data structure for AST
type AST = [[ByteString]]

annotateTokens :: [Token] -> AST -> [HolbornToken]
-- left merge
annotateTokens [] [] = []
annotateTokens (x:xs) (y:ys)
  | (tText x) == (y !! 0) = (HolbornToken x (Reference (decodeUtf8 (y !! 0)))) : (annotateTokens xs ys)
  | otherwise = (HolbornToken x (Reference "")) : annotateTokens xs (y:ys)
annotateTokens _ _ = []

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
renderPythonCode :: Text -> AST -> Html
renderPythonCode pythonCode ast =
  let simpleTokens = lexPythonCode pythonCode
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
