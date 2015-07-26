module Holborn.Types
       ( Symbol(..)
       , Annotation(..)
       , Reference(..)
       , HolbornToken(..)
       ) where

import BasicPrelude
import Text.Highlighter.Types (Token)

-- | A token with extra semantic information. More data to be added later.
data Reference = Reference Text deriving Show
data HolbornToken = HolbornToken Token Reference

-- | Placeholder data structure for AST
data Symbol = Symbol ByteString Reference deriving Show
data Annotation = Annotation [Symbol]
