module Holborn.Web
       ( HolbornToken
       , codePage
       , renderPythonCode
       , annotateTokens
       , leftMergeBy
       ) where

import BasicPrelude

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.List as List
import Data.Maybe (fromJust)

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Highlighter.Lexer (runLexer)
import Text.Highlighter.Lexers (lexers)
import Text.Highlighter.Types (Token(tText))
import Text.Show.Pretty (ppShow)

import Holborn.HtmlFormat (format)
import Holborn.Style (monokai)
import Holborn.Types (Annotation, HolbornToken(..))

import qualified Holborn.Python as P


leftMergeBy :: (a -> b -> Bool) -> [a] -> [b] -> Either [b] [(a, Maybe b)]
leftMergeBy _ [] [] = return []
leftMergeBy _ [] ys = Left ys
leftMergeBy _ xs [] = return [(x, Nothing) | x <- xs]
leftMergeBy match (x:xs) allY@(y:ys) = do
  let (matched, ys') = if match x y then (Just y, ys) else (Nothing, allY)
  rest <- leftMergeBy match xs ys'
  return $ (x, matched):rest



formatHtmlBlock :: (Show a, H.ToValue a) => [HolbornToken a] -> Html
formatHtmlBlock = format includeLineNumbers
  where includeLineNumbers = False


lexPythonCode :: Text -> [Token]
lexPythonCode code =
  fromRight $ runLexer pythonLexer (encodeUtf8 code)
  where pythonLexer = fromJust $ List.lookup ".py" lexers


assertRight :: Show a => Text -> Either a b -> b
assertRight _ (Right r) = r
assertRight message (Left e) = terror $ message ++ ": " ++ decodeUtf8 (pack (ppShow e))


fromRight :: Show a => Either a b -> b
fromRight (Left e) = terror $ show e
fromRight (Right r) = r


-- XXX: Actually handle cases where code won't lex or we can't find a lexer

-- XXX: Is Text the right type?

renderPythonCode :: Text -> Html
renderPythonCode code =
  let annotations = assertRight "Could not parse" (P.annotateSourceCode code)
      highlighterTokens = lexPythonCode code
      annotatedTokens = annotateTokens highlighterTokens annotations
  in
    formatHtmlBlock annotatedTokens


annotateTokens :: Show a => [Token] -> [(String, Maybe (Annotation a))] -> [HolbornToken a]
annotateTokens highlighterTokens semanticTokens =
  map (uncurry mergeTokens) (assertRight "Could not match AST data to tokenized data" mergeResult)
  where
    mergeResult = leftMergeBy matchToken highlighterTokens (justSecond semanticTokens)
    matchToken token (name, _) = unpack (tText token) == name
    mergeTokens token = HolbornToken token . map getReference
    getReference (_, annotation) = annotation
    justSecond = mapMaybe (\(x, y) -> (,) x <$> y)


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
