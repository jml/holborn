module Holborn.Browse where

import Prelude
import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Text.Parsing.Simple (Parser, string, alphanum, fromCharList, word)
import Text.Parsing.Combinators (many1)

import Web.Cookies as C
import React.DOM as R
import React.DOM.Props as RP
import Data.Lens (lens, LensP, set, view, toListOf)
import Data.Lens.Traversal (traversed)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo)
import Data.Lens.Types (Fold())
import Data.List (toUnfoldable, List)
import Data.Foldable (intercalate)
import Data.Array (range)

import Holborn.Fetchable (class Fetchable, fetch, Fetch, decodeResponse)
import Holborn.Config (makeUrl)
import Holborn.ManualEncoding.Browse (BrowseMetaResponse(..), GitTree(..), GitTreeEntry(..), GitBlob(..))
import Holborn.ManualEncoding.Browse as MB
import Holborn.Auth as Auth

import Debug.Trace

data State = State
    { route :: BrowseRoutes
    , _meta :: Maybe BrowseMetaResponse -- empty when not loaded
    , _tree :: Maybe GitTree -- Instead of maybe have a sum-type Nothing | Tree | Blob | Commit
    , _blob :: Maybe GitBlob -- sum type?
    }

startRoute :: BrowseRoutes -> State
startRoute s = State { route: s, _meta: Nothing, _tree: Nothing, _blob: Nothing }

data Action = NOP


-- TODO: I think toArrayOf has a bad runtime and can probably be
-- rewritten via FFI to append to mutable Array because of the
-- guarantees given by Traversable.
toArrayOf :: forall s t a b. Fold (Endo (List a)) s t a b -> s -> Array a
toArrayOf p s = toUnfoldable (toListOf p s)


-- | Fetch metadata for a git repository (e.g. description)
fetchMeta :: forall eff. BrowseRoutes -> Owner -> Repo -> State -> Fetch eff State
fetchMeta rt owner repo state = do
  let url = makeUrl ("/v1/repos/" ++ owner ++ "/" ++ repo)
  rx <- Auth.get url >>= decodeResponse
  let state' = set meta (Just rx) state
  fetch rt state'


makeTreeUrl :: String -> String -> Maybe String -> Maybe String -> String
makeTreeUrl owner repo ref path =
  makeUrl (intercalate "/" (
              ["/v1/repos", owner, repo]
              <> (maybe [] (\ref' -> ["git/trees", ref']) ref) <> (maybe [] (\path' -> [path']) path)))


makeBlobUrl :: String -> String -> String -> String -> String
makeBlobUrl owner repo ref path =
  makeUrl (intercalate "/" (
              ["/v1/repos", owner, repo, "git/blobs", ref, path]))


instance browseFetchable :: Fetchable BrowseRoutes State where
  fetch rt@(Home owner repo) state@(State { _meta = Nothing }) = fetchMeta rt owner repo state
  fetch rt@(Tree owner repo _ _) state@(State { _meta = Nothing }) = fetchMeta rt owner repo state
  fetch rt@(Blob owner repo _ _) state@(State { _meta = Nothing }) = fetchMeta rt owner repo state

  fetch (Home owner repo) state = do
    let url = makeTreeUrl owner repo (Just "master") Nothing
    treeJson <- Auth.get url >>= decodeResponse
    let state' = set routeLens (HomeLoaded owner repo) state
    pure (set tree (Just treeJson) state')

  fetch (Tree owner repo ref path) state = do
    let url = makeTreeUrl owner repo (Just ref) (Just path)
    treeJson <- Auth.get url >>= decodeResponse
    let state' = set routeLens (TreeLoaded owner repo ref path) state
    pure (set tree (Just treeJson) state')

  fetch (Blob owner repo ref path) state = do
    let url = makeBlobUrl owner repo ref path
    blobJson <- Auth.get url >>= decodeResponse
    let state' = set routeLens (BlobLoaded owner repo ref path) state
    pure (set blob (Just blobJson) state')

  fetch _ state = pure state


routeLens :: LensP State BrowseRoutes
routeLens = lens (\(State s) -> s.route) (\(State s) x -> State (s { route = x }))

meta :: LensP State (Maybe BrowseMetaResponse)
meta = lens (\(State s) -> s._meta) (\(State s) x -> State (s { _meta = x }))

tree :: LensP State (Maybe GitTree)
tree = lens (\(State s) -> s._tree) (\(State s) x -> State (s { _tree = x }))

blob :: LensP State (Maybe GitBlob)
blob = lens (\(State s) -> s._blob) (\(State s) x -> State (s { _blob = x }))

type Repo = String
type Owner = String
type RepoPath = String
type Ref = String


data BrowseRoutes =
  Home Owner Repo
  | HomeLoaded Owner Repo
  | Tree Owner Repo Ref RepoPath
  | TreeLoaded Owner Repo Ref  RepoPath
  | Blob Owner Repo Ref RepoPath
  | BlobLoaded Owner Repo Ref RepoPath


parseOwner :: Parser String
parseOwner = fromCharList <$> many1 alphanum
parseRepo :: Parser String
parseRepo =
  fromCharList <$> many1 alphanum <* (string "/" <|> string ".git" <|> string "")

-- TODO: parsePath & parseRef need some sophistication
parsePath :: Parser String
parsePath = word
parseRef :: Parser String
parseRef = fromCharList <$> many1 alphanum

-- NB the ordering of the parser is important, I can't make `eof` work.
browseRoutes :: Parser State
browseRoutes =
  map startRoute
    (     Tree <$> parseOwner <* string "/" <*> parseRepo <* string "tree/" <*> parseRef <* string "/" <*> parsePath
      <|> TreeLoaded <$> parseOwner <* string "/" <*> parseRepo <* string "tree/" <*> parseRef <* string "/" <*> parsePath
      <|> Blob <$> parseOwner <* string "/" <*> parseRepo <* string "blob/" <*> parseRef <* string "/" <*> parsePath
      <|> BlobLoaded <$> parseOwner <* string "/" <*> parseRepo <* string "blob/" <*> parseRef <* string "/" <*> parsePath
      <|> Home <$> parseOwner <* string "/" <*> parseRepo
      <|> HomeLoaded <$> parseOwner <* string "/" <*> parseRepo
    )

spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec T.defaultPerformAction render
  where
    render dispatch _ (State { route = Home org repo}) _ =
      [ R.h1 [] [R.text "browse ... (loading)"]
      , R.text $ org ++ repo
      ]
    render dispatch _ (State { route = HomeLoaded org repo, _meta = Just meta, _tree = Just tree }) _ =
      [ R.h1 [] [R.text "browse"]
      , R.h2 [] [R.text (view MB.description meta)]
      , R.ul [] (renderTree org repo tree)
      ]
    render dispatch _ (State { route = TreeLoaded org repo ref path, _meta = Just meta, _tree = Just tree }) _ =
      [ R.h1 [] [R.text "browse"]
      , R.h2 [] [R.text (view MB.description meta)]
      , R.ul [] (renderTree org repo tree)
      ]
    render dispatch _ (State { route = BlobLoaded org repo ref path, _meta = Just meta, _blob = Just (GitBlob blob) }) _ =
      [ R.h1 [] [R.text "browse"]
      , R.h2 [] [R.text (view MB.description meta)]
        -- TODO count number of lines
      , R.pre [RP.style {width: "5em", float: "left", textAlign: "right"}] (renderLines 10)
      , R.pre [] [R.text blob.contents]
      ]
    render dispatch _ _ _ =
      [ R.text "browse - not implemented probably a bug"
      ]

    -- TODO is renderLines very slow?
    renderLines :: Int -> Array React.ReactElement
    renderLines n = map (\i -> R.div [] [R.text (show i)]) (range 1 (n + 1))

    -- TODO tom: rendering with the parsed path is extremely
    -- fragile. Probably best to extract the path handling into a set
    -- of separate functions with tests.
    renderTree :: forall a. String -> String -> GitTree -> Array React.ReactElement
    renderTree org repo tree =
      let paths = MB.tree <<< traversed
          treePath = view MB.treePath tree
          entries = map (renderGitEntry org repo treePath) (toArrayOf paths tree)
      in entries

    renderGitEntry org repo treePath gte@(GitTreeEntry entry) =
      R.li []
      [ R.a [RP.href (makeLink org repo treePath gte)] [R.text entry.path]
      ]

    makeLink org repo treePath (GitTreeEntry entry) = case entry.type_ of
      "blob" -> intercalate "/" (["", org, repo, "blob", "master"] <> treePath <> [entry.path])
      "tree" -> intercalate "/" (["", org, repo, "tree", "master"] <> treePath <> [entry.path])
