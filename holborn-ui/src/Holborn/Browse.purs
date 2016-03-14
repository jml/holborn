module Holborn.Browse where

import Prelude
import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Routing.Match (Match)
import Routing.Match.Class (lit, str, fail)
import Web.Cookies as C
import React.DOM as R
import React.DOM.Props as RP
import Data.Lens(lens, LensP, set, view)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Holborn.Fetchable (class Fetchable, fetch)
import Holborn.Config (makeUrl)
import Holborn.ManualEncoding.Browse (BrowseMetaResponse(..), description)
import Holborn.Auth as Auth

import Debug.Trace

data State = State
    { route :: BrowseRoutes
    , _meta :: Maybe BrowseMetaResponse -- empty when not loaded
    }

startRoute s = State { route: s, _meta: Nothing }

data Action = NOP


instance browseFetchable :: Fetchable BrowseRoutes State where
  fetch (Home owner repo) state = do
    r <- Auth.get (makeUrl ("/v1/repos/" ++ owner ++ "/" ++ repo))
    newState <- case decodeJson r.response of
      Left err -> pure (set routeLens (HomeLoaded owner repo) state)
      Right browseMetaResponse ->
        let state' = (set routeLens (HomeLoaded owner repo) state)
        in pure (set meta (Just browseMetaResponse) state')

    -- TODO tree and metadata can be fetched in parallel (see Aff Par monad)
    rTree <- Auth.get (makeUrl ("/v1/repos/" ++ owner ++ "/" ++ repo ++ "/git/trees/master"))
    traceAnyM ( rTree.response :: String)
    pure newState


routeLens :: LensP State BrowseRoutes
routeLens = lens (\(State s) -> s.route) (\(State s) x -> State (s { route = x }))

meta :: LensP State (Maybe BrowseMetaResponse)
meta = lens (\(State s) -> s._meta) (\(State s) x -> State (s { _meta = x }))


data BrowseRoutes =
  Home String String
  | HomeLoaded String String


browseRoutes :: Match State
browseRoutes =
  map startRoute (Home <$> str <*> str)


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec T.defaultPerformAction render
  where
    render dispatch _ (State { route = Home org repo }) _ =
      [ R.h1 [] [R.text "browse ... (loading)"]
      , R.text $ org ++ repo
      ]
    render dispatch _ (State { route = HomeLoaded org repo, _meta = Just meta }) _ =
      [ R.h1 [] [R.text "browse"]
      , R.h2 [] [R.text (view description meta)]
      ]
    render dispatch _ _ _ =
      [ R.text "browse"
      ]
