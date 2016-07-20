-- | This task list application is made up of two Thermite components:
-- |
-- | - a component for a single task (Components.Task)
-- | - a component for a list of tasks (Components.TaskList)
-- |
-- | This example demonstrates the main features of a Thermite app:
-- |
-- | - Using `simpleSpec` to create simple components from `Render` and
-- |   `PerformAction` functions
-- | - Composing components using the `Monoid` instance and lens combinators.
-- |
-- | For each component we start by declaring action and state types.

module Main (main) where

import Prelude (Unit, (<$>), (<<<), bind, (>>=), void, pure, unit)

import Data.Nullable (toMaybe)

import Control.Monad.Eff (Eff)

import Components.Router as Router

import React as R
import ReactDOM as RD
import Control.Monad.Eff.Console (CONSOLE())

import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Maybe (Maybe(..))


-- | The main method creates the task list component, and renders it to the document body.
main :: forall eff. Eff (dom :: DOM.DOM, console :: CONSOLE | eff) Unit
main = do
  let reactElement = (R.createFactory Router.component {})
  document <- DOM.window >>= DOM.document
  container <- toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  case container of
    Just container' -> void (RD.render reactElement container')
    Nothing -> pure unit
