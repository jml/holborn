-- | holborn is a single-page app (SPA) and this is the main entry
-- point. We are loaded in a html page which contains exactly one div
-- with id `container` and place the app in that div.

module Main (main) where

import Prelude (Unit, (<$>), (<<<), bind, (>>=), void, pure, unit)

import Holborn.Router as Router
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE())
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import React as R
import ReactDOM as RD


main :: forall eff. Eff (dom :: DOM.DOM, console :: CONSOLE | eff) Unit
main = do
  let reactElement = (R.createFactory Router.component {})
  document <- DOM.window >>= DOM.document
  container <- toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  case container of
    Just container' -> void (RD.render reactElement container')
    Nothing -> pure unit
