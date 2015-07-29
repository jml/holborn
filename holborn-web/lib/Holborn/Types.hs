module Holborn.Types
       ( Identifier(..)
       , Annotation(..)
       , Reference(..)
       , HolbornToken(..)
       , tokenName
       , tokenReference
       , tokenType
       ) where

import BasicPrelude
import Text.Highlighter.Types (Token(..), TokenType)

-- | A token with extra semantic information. More data to be added later.
data HolbornToken = HolbornToken Token (Maybe Reference)

-- | Opaque data type representing the location of a token in our yet-to-be-
-- defined semantic data structure.
data Reference = Reference Text deriving Show

-- | An identifier found in code.
data Identifier = Identifier ByteString Reference deriving Show

-- | Placeholder data structure for AST
-- XXX: This doesn't represent an AST any more. What *does* it represent?
data Annotation = Annotation [Identifier]


tokenType :: HolbornToken -> TokenType
tokenType (HolbornToken (Token t _) _) = t


tokenName :: HolbornToken -> ByteString
tokenName (HolbornToken (Token _ s) _) = s


tokenReference :: HolbornToken -> Maybe Reference
tokenReference (HolbornToken _ r) = r
