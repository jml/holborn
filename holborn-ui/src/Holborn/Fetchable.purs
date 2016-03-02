module Holborn.Fetchable where

import Prelude
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AJ


class Fetchable a where
  fetch :: forall eff. a -> Aff (ajax :: AJ.AJAX | eff) a
