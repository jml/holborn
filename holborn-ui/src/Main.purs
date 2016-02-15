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

import Prelude

import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)

import Control.Monad.Eff

import qualified Components.Router as Router
import Holborn.Routing as HR

import qualified Thermite as T

import qualified React as R
import qualified ReactDOM as RD
import Control.Monad.Eff.Console (log, CONSOLE())

import Data.Maybe (Maybe())

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM

import Routing (matches)




-- | The main method creates the task list component, and renders it to the document body.
main :: forall eff. Eff (dom :: DOM.DOM, console :: CONSOLE | eff) Unit
main = void do

  let reactElement = (R.createFactory Router.component {})

  document <- DOM.window >>= DOM.document
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  RD.render reactElement container
