module Holborn.Syntax.Types
       ( Annotation(..)
       , AnnotatedSource(..)
       , Identifier(..)
       , HolbornToken(..)
       , tokenAnnotation
       , tokenName
       , tokenShortName
       , tokenType
       ) where

import BasicPrelude
import Text.Highlighter.Types (Token(..), TokenType, shortName)


-- | A file that has been tokenized and annotated.
data AnnotatedSource a = AnnotatedSource { getTokens :: [HolbornToken a] }

-- | A token with extra semantic information. More data to be added later.
data HolbornToken a = HolbornToken { _lexerToken :: Token
                                   , _annotation :: Maybe (Annotation a)
                                   }


-- | Annotation applied to a token. We are interested only in identifiers and
-- how they are used: is an identifier being defined, or is it a reference?
--
-- For definitions, we store something that will help us find the definition
-- again. For references, we store the location of the definition.
data Annotation a = Binding a
                  | Reference a
                  | UnresolvedReference
                  deriving (Eq, Show)


-- | An identifier found in code.
--
-- Currently only used in example code.
data Identifier a = Identifier ByteString a deriving Show


tokenType :: HolbornToken a -> TokenType
tokenType (HolbornToken (Token t _) _) = t


tokenName :: HolbornToken a -> ByteString
tokenName (HolbornToken (Token _ s) _) = s


tokenShortName :: IsString b => HolbornToken a -> b
tokenShortName = fromString . shortName . tokenType


tokenAnnotation :: HolbornToken a -> Maybe (Annotation a)
tokenAnnotation = _annotation
