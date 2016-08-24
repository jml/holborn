-- | Router that uses a parser instead of custom combinators. See
-- https://pursuit.purescript.org/packages/purescript-simple-parser/2.1.0
-- for supported combinators.
module Standalone.Router.Dispatch where

import Prelude
import Text.Parsing.Simple (unparser, Parser)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Either (Either(..))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)


foreign import data Navigate :: !


-- | Takes callback  `oldroute -> newroute -> Eff e Unit`
foreign import routeChanged :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit

-- | See https://developer.mozilla.org/en-US/docs/Web/API/History/pushState
-- e.g. history.pushState({}, "", "/hello"). Notes:
-- 1/ We're only using the URL component, not the state ATM
-- 2/ Will manually fire routeChanged
-- 3/ Call this instead of using "<a href=..."
foreign import pushState :: forall e. String -> Eff e Unit


-- | Push new path to history.
navigate :: forall e. String -> Eff (navigate :: Navigate | e) Unit
navigate = pushState


-- purescript/purescript#2061
liftEff2 = liftEff :: forall eff a. Eff eff a -> Aff eff a

-- | Needed to make the type checker happy
navigateA :: forall e. String -> Aff (navigate :: Navigate | e) Unit
navigateA s = liftEff2 (pushState s)


-- | TODO do we want leave / enter events instead of Maybe a?
matches :: forall a e. Parser String a -> (a -> Eff e Unit) -> Eff e Unit
matches parser callback = do
  routeChanged $ \old new ->
    let result = unparser parser new in
    case result.remaining of
      "" -> case result.consumed of
        Right x -> callback x
        Left _ -> unsafeThrow "You forgot an always matching 404 route in your route parser."
      _ -> unsafeThrow ("Not all of URL consumed. Remaining: " <> result.remaining)
