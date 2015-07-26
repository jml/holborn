module Holborn.Types
       ( Symbol(..)
       , Annotation(..)
       , Reference(..)
       , HolbornToken(..)
       ) where

import BasicPrelude
import Text.Highlighter.Types (Token)

-- | A token with extra semantic information. More data to be added later.
data HolbornToken = HolbornToken Token Reference

-- | Opaque data type representing the location of a token in our yet-to-be-
-- defined semantic data structure.
data Reference = Reference Text

-- | An identifier found in code.
data Symbol = Symbol ByteString Reference

-- | Placeholder data structure for AST
data Annotation = Annotation [Symbol]
