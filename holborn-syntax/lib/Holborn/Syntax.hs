{-| Analyze source code and generate linkified versions.

This module is the main entry point to the rest of the library.

-}
module Holborn.Syntax
       ( HolbornSource
       , annotatePythonCode
       ) where

import BasicPrelude

import Control.Error (fmapL, note)
import Data.ByteString.Char8 (unpack)
import qualified Data.List as List
import Text.Highlighter.Lexer (runLexer)
import Text.Highlighter.Lexers (lexers)
import Text.Highlighter.Types (Token(tText))
import PrettyError (assertRight, fromRight)

import Holborn.Internal (leftMergeBy)
import qualified Holborn.Python as P
import Holborn.Scope (ID)
import Holborn.Types (Annotation, AnnotatedSource(..), HolbornToken(..))


-- | Source code that we've annotated with our intelligent parser.
type HolbornSource = AnnotatedSource ID


annotatePythonCode :: Text -> HolbornSource
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
