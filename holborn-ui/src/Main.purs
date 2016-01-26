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
import Control.Monad.Eff.Console (log, CONSOLE())
import Data.Maybe (Maybe())

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM

import Routing (matches)


-- The following is a hack to listen on route changes for the "root"
-- component that controls everything else. `dispatch` can be
-- extracted from the spec but takes a `this` pointer which is only
-- valid once we mounted a component.
componentDidMount :: forall props state eff. (React.ReactThis props state -> Router.Action -> T.EventHandler) -> R.ComponentDidMount props state (console :: CONSOLE | eff)
componentDidMount dispatch this = do
    matches HR.rootRoutes callback
  where
    callback :: Maybe HR.RootRoutes -> HR.RootRoutes -> T.EventHandler
    callback _ rt = do
      dispatch this (Router.UpdateRoute rt)


-- | The main method creates the task list component, and renders it to the document body.
main :: forall eff. Eff (dom :: DOM.DOM, console :: CONSOLE | eff) Unit
main = void do

  -- Demo for how to hook into life cycle.
  let rspec = T.createReactSpec Router.spec Router.initialState
  let component = R.createClass ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec))})
  let reactElement = (R.createFactory component {})

  document <- DOM.window >>= DOM.document
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  R.render reactElement container
