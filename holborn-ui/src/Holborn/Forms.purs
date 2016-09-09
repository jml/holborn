module Holborn.Forms where

import Prelude

import React as React
import React.DOM as R
import React.DOM.Props as RP
import Data.Lens (LensP, view, set)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..), maybe)

import Data.Lens.Prism (review)
import Data.Lens.Prism.Maybe (_Just)

import Debug.Trace

inp :: forall eff p s r formData.
       String -- input type
       -> String -- input label
       -> Maybe String -- error
       -> LensP formData (Maybe String)
       -> (formData -> React.EventHandlerContext eff p s r)
       -> formData
       -> React.ReactElement
inp _type label error lens dp state =
  R.div [RP.className ("form-group" <> maybe "" (const " has-error") error) ]
  [ R.label [ RP.htmlFor label] [R.text (maybe label id error)]
  , R.input [ RP._type _type
            , RP._id label
            , RP.value (view (lens <<< _Just) state)
            , RP.className "form-control"
            , RP.onChange \ev -> dp (set lens (Just (unsafeCoerce ev).target.value) state)
            ] []
  ]

text = inp "text"
password = inp "password"


textarea :: forall eff p s r formData.
       String -- label
       -> Maybe String -- error
       -> LensP formData (Maybe String)
       -> (formData -> React.EventHandlerContext eff p s r)
       -> formData
       -> React.ReactElement
textarea label error lens dp state =
  R.div [RP.className ("form-group" <> maybe "" (const " has-error") error) ]
  [ R.label [ RP.htmlFor label] [R.text (maybe label id error)]
  , R.textarea [ RP._id label
               , RP.value (view (lens <<< _Just) state)
               , RP.className "form-control"
               , RP.onChange \ev -> dp (set lens (Just (unsafeCoerce ev).target.value) state)
               ] []
  ]
