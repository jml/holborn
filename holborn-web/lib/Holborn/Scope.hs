-- | Very simple model for lexical scopes.

module Holborn.Scope ( Scoped
                     , execScoped
                     , pushScope
                     , popScope
                     , newScope
                     , ID
                     , Annotation(..)
                     , addReference
                     , bind
                     , flattenScope
                     ) where

import BasicPrelude

import Control.Monad.State (State, get, modify, state, runState)
import qualified Data.Map as M

import Holborn.Types (Annotation(..))


type Symbol = String
type ID = Int

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

-- XXX: I don't have a way of handling redefinition within scope
data Environment a = Env { definitions :: Map Symbol (ID, a)
                         , references :: [(ID, a)]
                         }

newEnvironment :: Environment a
newEnvironment = Env M.empty []


flattenEnvironment :: Ord a => Environment a -> Map a (Annotation ID)
flattenEnvironment env =
  M.fromList $
  [(srcSpan, Binding i) | (i, srcSpan) <- M.elems (definitions env)] ++
  [(srcSpan, Reference i) | (i, srcSpan) <- references env]


insertBinding :: Symbol -> ID -> a -> Environment a -> Environment a
insertBinding symbol bindID srcSpan (Env env refs) = Env (M.insert symbol (bindID, srcSpan) env) refs

insertReference :: Symbol -> ID -> a -> Environment a -> Environment a
insertReference _ refID srcSpan (Env env refs) = Env env ((refID, srcSpan):refs)


getBinding :: Symbol -> Environment a -> Maybe ID
getBinding symbol environment = fst <$> M.lookup symbol (definitions environment)


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


pushScope :: Scoped a ()
pushScope = modify (pushEnvironment newEnvironment)


popScope :: Scoped a (Environment a)
popScope = state popEnvironment'


incrementID :: Scoped a ID
incrementID = state incrementID'


bind :: Symbol -> a -> Scoped a ID
bind symbol srcSpan = do
  nextID <- incrementID
  modify (modifyEnvironment (insertBinding symbol nextID srcSpan))
  return nextID


addReference :: Symbol -> a -> Scoped a ()
addReference symbol srcSpan = do
  definition <- findDefinition symbol <$> get
  case definition of
    Just i -> modify (modifyEnvironment (insertReference symbol i srcSpan))
    -- XXX: Have something nicer for references that we can't resolve
    _ -> return ()
