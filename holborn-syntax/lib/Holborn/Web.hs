module Holborn.Web
       ( HolbornToken
       , codePage
       , annotatePythonCode
       , annotateTokens
       ) where

import BasicPrelude

import Control.Error (fmapL, note)
import Data.ByteString.Char8 (unpack)
import qualified Data.List as List

import PrettyError (assertRight, fromRight)

import Text.Blaze (ToMarkup)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Highlighter.Lexer (runLexer)
import Text.Highlighter.Lexers (lexers)
import Text.Highlighter.Types (Token(tText))

import Holborn.Internal (leftMergeBy)
import Holborn.Scope (ID)
import Holborn.Style (monokai)
import Holborn.Types (Annotation, AnnotatedSource(..), HolbornToken(..))

import qualified Holborn.Python as P


-- TODO: Move these to the Python module
annotatePythonCode :: Text -> AnnotatedSource ID
annotatePythonCode code = fromRight $ do
  pythonLexer <- note "Could not load Python lexer" $ List.lookup ".py" lexers
  highlighterTokens <- fmapL show $ runLexer pythonLexer (encodeUtf8 code)
  annotations <- fmapL show $ P.annotateSourceCode code
  return $ annotateTokens highlighterTokens annotations


annotateTokens :: Show a => [Token] -> [(String, Maybe (Annotation a))] -> AnnotatedSource a
annotateTokens highlighterTokens semanticTokens =
  AnnotatedSource $ map (uncurry mergeTokens) (assertRight "Could not match AST data to tokenized data" mergeResult)
  where
    mergeResult = leftMergeBy matchToken highlighterTokens (justSecond semanticTokens)
    matchToken token (name, _) = unpack (tText token) == name
    mergeTokens token = HolbornToken token . map getReference
    getReference (_, annotation) = annotation
    justSecond = mapMaybe (\(x, y) -> (,) x <$> y)


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
