module Standalone.Thermite (nestSpec) where

import Prelude (bind)

import Thermite as T
import Data.Lens (view)

-- | Nest two specs and return a new spec.
--
-- The outer spec can use `children` (last argument to render) to
-- embed the child spec.
nestSpec :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action -> T.Spec eff state props action
nestSpec parent child = T.simpleSpec performAction render
 where
   performAction a p st = do
     view T._performAction parent a p st
     view T._performAction child a p st
   render k p st children = view T._render parent k p st (view T._render child k p st children)
