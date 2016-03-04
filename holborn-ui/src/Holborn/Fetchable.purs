module Holborn.Fetchable where

import Prelude
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AJ

-- | Route changes generally imply the need to fetch fresh data from
-- the server (e.g. account data) but we don't know what routes and
-- nested state we have in advance (they can be nested). So they can
-- implement how to fetch themselves via this class.
class Fetchable action state where
  fetch :: forall eff. action -> state -> Aff (ajax :: AJ.AJAX | eff) state
