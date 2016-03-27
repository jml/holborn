module Holborn.DomHelpers where

import Prelude (Unit)
import Control.Monad.Eff (Eff)

foreign import scroll :: forall e. Int -> Int -> Eff e Unit
