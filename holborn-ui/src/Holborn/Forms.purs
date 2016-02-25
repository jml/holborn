module Holborn.Forms where

import Prelude

import React as React
import React.DOM as R
import React.DOM.Props as RP
import Data.Lens (LensP, view, set)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..), maybe)

inp :: forall eff p s r formData.
       String
       -> String -- | label
       -> Maybe String -- | error
       -> LensP formData String
       -> (formData -> React.EventHandlerContext eff p s r)
       -> formData
       -> React.ReactElement
inp _type label error lens dp state =
  R.div [RP.className ("form-group" ++ maybe "" (const " has-error") error) ]
  [ R.label [ RP.htmlFor label] [R.text label]
  , R.input [ RP._type _type
            , RP._id label
            , RP.value (view lens state)
            , RP.className "form-control"
            , RP.onChange \ev -> dp (set lens (unsafeCoerce ev).target.value state)
            ] []
  ]

text = inp "text"
password = inp "password"
