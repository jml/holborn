module Holborn.Forms where

import Prelude

import React as React
import React.DOM as R
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..), maybe)

import Data.Lens (review, _Just, Optic, Forget, IsoP, SetterP, TraversalP, preview, GetterP, LensP, view, set, matching)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Either (Either(..))
import Debug.Trace



inp :: forall eff props state r formData s t b p.
       String -- input type
       -> String -- input label
       -> Maybe String -- error
       -> TraversalP formData (Maybe String)
       -> (formData -> React.EventHandlerContext eff props state r)
       -> formData
       -> React.ReactElement
inp _type label error lens dp state =
  R.div [RP.className ("form-group" <> maybe "" (const " has-error") error) ]
  [ R.label [ RP.htmlFor label] [R.text (maybe label id error)]
  , R.input [ RP._type _type
            , RP._id label
            , RP.value (view (lens <<< _Just) state)
            , RP.className "form-control"
            , RP.onChange \ev -> dp (set lens (Just (spy (unsafeCoerce ev).target.value)) state)
            ] []
  ]

text :: _
text = inp "text"

password :: _
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
