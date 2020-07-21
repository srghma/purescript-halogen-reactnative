module Halogen.ReactNative
  ( slot
  , slot'
  , module Halogen.ReactNative.Core
  , module Halogen.ReactNative.Elements
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Component (Component, mkComponentSlot, unComponent)
import Halogen.ReactNative.Core (ParentVIEW, VIEW, ComponentVIEW)
import Halogen.ReactNative.Core as Core
import Halogen.ReactNative.Elements (button, scrollView, text, text', textInput, view)

-- | Defines a slot for a child component. Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot
  :: forall query action input output slots m label slot _1
   . Row.Cons label (Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> Component query input output m
  -> input
  -> (output -> action)
  -> ParentVIEW action slots m
slot label p component input outputQuery =
  let f = unComponent _.receiver component
  in VIEW (Widget (mkComponentSlot p component input f outputQuery Just))

-- | slot
-- |   :: forall f m p i o g
-- |   . p
-- |  -> Component VIEW g i o m
-- |  -> i
-- |  -> (o -> Maybe (f Unit))
-- |  -> ParentVIEW f g p m
-- | slot p component input outputQuery =
-- |   let f = unComponent _.receiver component
-- |   in Core.slot (mkComponentSlot p component input f outputQuery Just)
