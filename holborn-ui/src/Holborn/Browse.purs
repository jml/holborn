module Holborn.Browse where

import Prelude
import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Text.Parsing.Simple (Parser, string, alphanum, fromCharList, eof, word)
import Text.Parsing.Combinators (many1)

import Web.Cookies as C
import React.DOM as R
import React.DOM.Props as RP
import Data.Lens (lens, LensP, set, view, toListOf)
import Data.Lens.Traversal (traversed)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo)
import Data.Lens.Types (Fold())
import Data.List (toUnfoldable, List)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Foldable (intercalate)

import Holborn.Fetchable (class Fetchable, fetch)
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
startRoute s = State { route: s, _meta: Nothing, _tree: Nothing }

data Action = NOP


buggyServer = unsafeThrow

-- TODO: I think toArrayOf has a bad runtime and can probably be
-- rewritten via FFI to append to mutable Array because of the
-- guarantees given by Traversable.
toArrayOf :: forall s t a b. Fold (Endo (List a)) s t a b -> s -> Array a
toArrayOf p s = toUnfoldable (toListOf p s)


instance browseFetchable :: Fetchable BrowseRoutes State where
  fetch rt@(Home owner repo path) state@(State { _meta = Nothing }) = do
    r <- Auth.get (makeUrl ("/v1/repos/" ++ owner ++ "/" ++ repo))
    newState <- case decodeJson r.response of
      Left err -> buggyServer err
      Right browseMetaResponse ->
        pure (set meta (Just browseMetaResponse) state)

    fetch rt newState

  fetch (Home owner repo path) state = do
    let url = makeUrl ("/v1/repos/" ++ owner ++ "/" ++ repo ++ "/git/trees/master" ++ (maybe "" id path))
    rTree <- Auth.get url
    case decodeJson rTree.response of
      Left err -> buggyServer err
      Right treeResponse ->
        let state' = set routeLens (HomeLoaded owner repo path) state
        in pure (set tree (Just treeResponse) state')

  fetch _ state = pure state


routeLens :: LensP State BrowseRoutes
routeLens = lens (\(State s) -> s.route) (\(State s) x -> State (s { route = x }))

meta :: LensP State (Maybe BrowseMetaResponse)
meta = lens (\(State s) -> s._meta) (\(State s) x -> State (s { _meta = x }))

tree :: LensP State (Maybe GitTree)
tree = lens (\(State s) -> s._tree) (\(State s) x -> State (s { _tree = x }))

type Repo = String
type Owner = String
type RepoPath = String

data BrowseRoutes =
  Home Owner Repo (Maybe RepoPath)
  | HomeLoaded Owner Repo (Maybe RepoPath)

parseOwner = fromCharList <$> many1 alphanum
parseRepo =
  fromCharList <$> many1 alphanum <* ((string "/" <* eof) <|> string ".git" <|> string "")

-- TODO: parsePath needs some sophistication
parsePath = word

browseRoutes :: Parser State
browseRoutes =
  map startRoute
    ( Home <$> parseOwner <* string "/" <*> parseRepo <*> map Just parsePath
      <|> Home <$> parseOwner <* string "/" <*> parseRepo <*> pure Nothing
    )


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec T.defaultPerformAction render
  where
    render dispatch _ (State { route = Home org repo path}) _ =
      [ R.h1 [] [R.text "browse ... (loading)"]
      , R.text $ org ++ repo
      ]
    render dispatch _ (State { route = HomeLoaded org repo path, _meta = Just meta, _tree = Just tree }) _ =
      [ R.h1 [] [R.text "browse"]
      , R.h2 [] [R.text (view MB.description meta)]
      , R.ul [] (renderTree org repo tree)
      ]
    render dispatch _ _ _ =
      [ R.text "browse"
      ]

    -- TODO tom: rendering with the parsed path is extremely
    -- fragile. Probably best to extract the path handling into a set
    -- of separate functions with tests.
    renderTree :: forall a. String -> String -> GitTree -> Array React.ReactElement
    renderTree org repo tree =
      let paths = MB.tree <<< traversed
          treePath = view MB.treePath tree
          entries = map (renderGitEntry org repo treePath) (toArrayOf paths tree)
      in entries

    renderGitEntry org repo treePath (GitTreeEntry entry) =
      R.li []
      [ R.a [RP.href (intercalate "/" (["", org, repo] <> treePath <> [entry.path]))] [R.text entry.path]
      ]
