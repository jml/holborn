module Holborn.Browse where

import Prelude
import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Text.Parsing.Simple (Parser, string, alphanum, fromCharList)
import Text.Parsing.Combinators (many1)

import Web.Cookies as C
import React.DOM as R
import React.DOM.Props as RP
import Data.Lens (lens, LensP, set, view, toListOf)
import Data.Lens.Traversal (traversed)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo)
import Data.Lens.Types (Fold())
import Data.List (toUnfoldable, List)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Foldable (intercalate)

import Holborn.Fetchable (class Fetchable)
import Holborn.Config (makeUrl)
import Holborn.ManualEncoding.Browse (BrowseMetaResponse(..), GitTree(..), GitTreeEntry(..))
import Holborn.ManualEncoding.Browse as MB
import Holborn.Auth as Auth

import Debug.Trace

data State = State
    { route :: BrowseRoutes
    , _meta :: Maybe BrowseMetaResponse -- empty when not loaded
    , _tree :: Maybe GitTree -- Instead of maybe have a sum-type Nothing | Tree | Blob | Commit
    }

startRoute :: BrowseRoutes -> State
startRoute s = State { route: spy s, _meta: Nothing, _tree: Nothing }

data Action = NOP


-- TODO: I think toArrayOf has a bad runtime and can probably be
-- rewritten via FFI to append to mutable Array because of the
-- guarantees given by Traversable.
toArrayOf :: forall s t a b. Fold (Endo (List a)) s t a b -> s -> Array a
toArrayOf p s = toUnfoldable (toListOf p s)


instance browseFetchable :: Fetchable BrowseRoutes State where
  fetch (Home owner repo) state = do
    r <- Auth.get (makeUrl ("/v1/repos/" ++ owner ++ "/" ++ repo))
    newState <- case decodeJson r.response of
      Left err -> unsafeThrow "could not decode main"
      Right browseMetaResponse ->
        let state' = (set routeLens (HomeLoaded owner repo) state)
        in pure (set meta (Just browseMetaResponse) state')

    -- TODO tree and metadata can be fetched in parallel (see Aff Par monad)
    rTree <- Auth.get (makeUrl ("/v1/repos/" ++ owner ++ "/" ++ repo ++ "/git/trees/master"))
    case decodeJson rTree.response of
      Left err -> unsafeThrow err
      Right treeResponse -> do
        pure (set tree (Just treeResponse) newState)

  fetch _ state = pure state


routeLens :: LensP State BrowseRoutes
routeLens = lens (\(State s) -> s.route) (\(State s) x -> State (s { route = x }))

meta :: LensP State (Maybe BrowseMetaResponse)
meta = lens (\(State s) -> s._meta) (\(State s) x -> State (s { _meta = x }))

tree :: LensP State (Maybe GitTree)
tree = lens (\(State s) -> s._tree) (\(State s) x -> State (s { _tree = x }))


data BrowseRoutes =
  Home String String
  | HomeLoaded String String


parseOwner = fromCharList <$> many1 alphanum
parseRepo = fromCharList <$> many1 alphanum

browseRoutes :: Parser State
browseRoutes =
  map startRoute (Home <$> parseOwner <* string "/" <*> parseRepo)


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec T.defaultPerformAction render
  where
    render dispatch _ (State { route = Home org repo }) _ =
      [ R.h1 [] [R.text "browse ... (loading)"]
      , R.text $ org ++ repo
      ]
    render dispatch _ (State { route = HomeLoaded org repo, _meta = Just meta, _tree = Just tree }) _ =
      [ R.h1 [] [R.text "browse"]
      , R.h2 [] [R.text (view MB.description meta)]
      , R.ul [] (renderTree org repo tree)
      ]
    render dispatch _ _ _ =
      [ R.text "browse"
      ]

    renderTree org repo tree =
      let paths = MB.tree <<< traversed
      in map (renderGitEntry org repo) (toArrayOf paths tree)

    renderGitEntry org repo (GitTreeEntry entry) =
      R.li []
      [ R.a [RP.href (intercalate "/" ["", org, repo, entry.path])] [R.text entry.path]
      ]
