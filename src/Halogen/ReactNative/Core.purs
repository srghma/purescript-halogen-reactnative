module Halogen.ReactNative.Core
  ( VIEW(..)
  , Native(..)
  , ComponentVIEW
  , ParentVIEW
  , Graft(..)
  , GraftX(..)
  , unGraft
  , graft
  , runGraft
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Halogen.Query.Input (Input)
import Halogen.Component (ComponentSlot)
import ReactNative.Basic (NativeClass, Prop)
import Unsafe.Coerce (unsafeCoerce)

type ParentVIEW action slots m = VIEW (ComponentSlot VIEW slots m action) action

newtype VIEW w i = VIEW (Native (Array (Prop (Input i))) w)

instance bifunctorVIEW :: Bifunctor VIEW where
  bimap f g (VIEW vdom) = VIEW (bimap (map (map (map g))) f vdom)

instance functorVIEW :: Functor (VIEW w) where
  map = rmap

type ComponentVIEW f = VIEW Void Void

-- | `a` is the type of attributes,
data Native a w
  = Elem NativeClass a (Array (Native a w))
  | Text String
  | Widget w
  | Grafted (Graft a w)

instance bifunctorNative ∷ Bifunctor Native where
  bimap f g (Text a) = Text a
  bimap f g (Grafted a) = Grafted (bimap f g a)
  bimap f g a = Grafted (graft (Graft f g a))

instance functorNative :: Functor (Native a) where
  map = rmap

-- Not sure what this is yet, but it looks important
foreign import data Graft ∷ Type → Type → Type

instance functorGraft ∷ Functor (Graft a) where
  map g = unGraft \(Graft f' g' a) → graft (Graft f' (g <<< g') a)

instance bifunctorGraft ∷ Bifunctor Graft where
  bimap f g = unGraft \(Graft f' g' a) → graft (Graft (f <<< f') (g <<< g') a)

data GraftX a a' w w' =
  Graft (a → a') (w → w') (Native a w)

graft
  ∷ ∀ a a' w w'
  . GraftX a a' w w'
  → Graft a' w'
graft = unsafeCoerce

unGraft
  ∷ ∀ a' w' r
  . (∀ a w. GraftX a a' w w' → r)
  → Graft a' w'
  → r
unGraft f = f <<< unsafeCoerce

runGraft
  ∷ ∀ a' w'
  . Graft a' w'
  → Native a' w'
runGraft =
  unGraft \(Graft fa fw v) →
    let
      go (Text s) = Text s
      go (Widget w) = Widget (fw w)
      go (Elem nativeClass p ch) = Elem nativeClass (fa p) (map go ch)
      go (Grafted g) = Grafted (bimap fa fw g)
    in
      go v
