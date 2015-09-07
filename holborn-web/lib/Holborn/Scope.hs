{-# LANGUAGE MultiParamTypeClasses #-}

{-| Simple model for lexical scopes.

Intended to work for any programming language, but currently only has Python
use case.

Languages provide their own AST objects and implement 'Interpreter' for them.
The 'interpret' function then specifies how to traverse the AST. The major
operations are:

 * 'bind'
 * 'addReference'
 * 'enterScope'
 * 'exitScope'

'calculateAnnotations' then takes an interpreter and produces a map of code
locations to 'Annotation' values.

== Missing

Currently missing support for a few necessary features

 * re-binding a variable within a scope
 * unbinding a variable
 * declaring different models for variables within a scope (e.g. Python's
   @global@).
 * support for unresolved references

-}

module Holborn.Scope ( Scoped
                     , calculateAnnotations
                     , enterScope
                     , exitScope
                     , ID
                     , addReference
                     , bind
                     , Interpreter(..)
                     ) where

import BasicPrelude

import Control.Monad.State (State, get, modify, state, runState)
import qualified Data.Map as M

import Holborn.Types (Annotation(..))


-- | @m@ is the kind of thing, @location@ is the location type
--
-- @location@ must be something that uniquely identifies the thing being
-- interpreted so that we can match these annotations to syntax-highlighted
-- tokens.
class Interpreter m location where

  -- | Interpret an expression.
  interpret :: m location -> Scoped location ()

  -- | Interpret the expression if it's there.
  interpretMaybe :: Interpreter m a => Maybe (m a) -> Scoped a ()
  interpretMaybe = maybe (return ()) interpret

  -- | Interpret a sequence of things.
  interpretSequence :: (Foldable t, Interpreter m a) => t (m a) -> Scoped a ()
  interpretSequence = mapM_ interpret


-- XXX: I kind of like the 'interpret' abstraction, but it's a little general.
-- I'd like to be able to express in the type system that certain expressions
-- *cannot* create bindings.
--
-- Another way of thinking about the problem is that a binding is a "write" to
-- a namespace, and that a reference is a "read". Interpreting expressions
-- should be a read-only operation.


-- | Given an AST, return a map from locations to annotations.
calculateAnnotations :: (Ord a, Interpreter m a) => m a -> Map a (Annotation ID)
calculateAnnotations ast = flattenScope $ execScoped (interpret ast) newScope


type ID = Int
type Symbol = Text


-- Natural way to build the model is to have a stack of environments, and
-- every time we come across a binding to insert that into the model, and also
-- to note every time we come across a reference.
--
-- We want to then be able to do a second pass that just looks at the tokens
-- and then consults this 'database' to see whether it is a definition or a
-- reference or nothing
--
-- Therefore, the database needs to be indexed by something that we have when
-- we have the tokens:
--   probably source location
--
-- Therefore, when we insert the definitions or references, we need to make
-- note of their source location
--
-- There will be some things which are ambiguous references, most notably,
-- references to attributes on objects. We can think of those references as
-- partial functions that can only be resolved when given other information.


data Environment a =
  Env {
    -- | The definitions that have been made in this environment. Because a
    -- symbol can be redefined within an environment, we store a list. The
    -- first element of the list is the latest binding. The list is never
    -- empty.
    definitions :: Map Symbol [(ID, a)],
    -- | References found in the environment. @Just x@ means we could find a
    -- matching symbol in scope, @Nothing@ means we couldn't.
    references :: [(Maybe ID, a)]
    }

newEnvironment :: Environment a
newEnvironment = Env M.empty []


flattenEnvironment :: Ord a => Environment a -> Map a (Annotation ID)
flattenEnvironment env =
  M.fromList $ bindings ++ map getReference (references env)
  where
    bindings = do
      xs <- M.elems (definitions env)
      (i, srcSpan) <- xs
      return (srcSpan, Binding i)
    getReference (Just i, srcSpan) = (srcSpan, Reference i)
    getReference (Nothing, srcSpan) = (srcSpan, UnresolvedReference)


insertBinding :: Symbol -> ID -> a -> Environment a -> Environment a
insertBinding symbol bindID srcSpan (Env env refs) =
  Env (addToKey symbol (bindID, srcSpan) env) refs


addToKey :: Ord k => k -> a -> Map k [a] -> Map k [a]
addToKey key value =
  M.alter f key
  where f = Just . (value:) . fromMaybe []


insertReference :: Symbol -> Maybe ID -> a -> Environment a -> Environment a
insertReference _ refID srcSpan (Env env refs) = Env env ((refID, srcSpan):refs)


getBinding :: Symbol -> Environment a -> Maybe ID
getBinding symbol =
  listToMaybe . map fst . fromMaybe [] . M.lookup symbol . definitions


data Scope a = Scope { _stack :: [Environment a]
                     , _root :: Environment a
                     , _currentID :: ID
                     , _past :: [Environment a]
                     }


-- XXX: I don't understand why I can't just make Scope implement Foldable. I
-- think it's because that's for structures that don't know anything about
-- their contents. Calling this foldMapScope to disambiguate, as some versions
-- of basic-prelude export it.
foldMapScope :: Monoid m => (Environment a -> m) -> Scope a -> m
foldMapScope f scope =
  case popEnvironment scope of
    (Left env, _) -> f env
    (Right env, scope') -> f env `mappend` foldMapScope f scope'


newScope :: Scope a
newScope = Scope [] newEnvironment 1 []

flattenScope :: Ord a => Scope a -> Map a (Annotation ID)
flattenScope scope = foldMapScope flattenEnvironment scope `mappend` mconcat (map flattenEnvironment (_past scope))

findDefinition :: Symbol -> Scope a -> Maybe ID
findDefinition symbol = listToMaybe . foldMapScope (maybeToList . getBinding symbol)

pushEnvironment :: Environment a -> Scope a -> Scope a
pushEnvironment env (Scope stack root x past) = Scope (env:stack) root x past

popEnvironment :: Scope a -> (Either (Environment a) (Environment a), Scope a)
popEnvironment scope@(Scope [] root _ _) = (Left root, scope)
popEnvironment (Scope (env:rest) root x past) = (Right env, Scope rest root x (env:past))

popEnvironment' :: Scope a -> (Environment a, Scope a)
popEnvironment' scope =
  let (env, scope') = popEnvironment scope in
  case env of
    Left env' -> (env', scope')
    Right env' -> (env', scope')

modifyEnvironment :: (Environment a -> Environment a) -> Scope a -> Scope a
modifyEnvironment f (Scope [] root x past) = Scope [] (f root) x past
modifyEnvironment f (Scope (env:rest) root x past) = Scope (f env:rest) root x past

incrementID' :: Scope a -> (ID, Scope a)
incrementID' (Scope stack root i past) = (i, Scope stack root (i + 1) past)


type Scoped a = State (Scope a)


runScoped :: Scoped location result -> Scope location -> (result, Scope location)
runScoped = runState


execScoped :: Scoped location result -> Scope location -> Scope location
execScoped action = snd . runScoped action


-- | We have entered a new scope within the present one.
enterScope :: Scoped a ()
enterScope = modify (pushEnvironment newEnvironment)


-- | We have exited the current scope.
exitScope :: Scoped a (Environment a)
exitScope = state popEnvironment'


incrementID :: Scoped a ID
incrementID = state incrementID'


-- | Declare that a symbol is bound at location in the current scope.
bind :: Symbol -> location -> Scoped location ID
bind symbol srcSpan = do
  nextID <- incrementID
  modify (modifyEnvironment (insertBinding symbol nextID srcSpan))
  return nextID


-- | Declare that symbol is a reference to a previously bound variable, using
-- the current scope to figure out precisely which variable.
addReference :: Symbol -> a -> Scoped a ()
addReference symbol srcSpan = do
  definition <- findDefinition symbol <$> get
  modify (modifyEnvironment (insertReference symbol definition srcSpan))
