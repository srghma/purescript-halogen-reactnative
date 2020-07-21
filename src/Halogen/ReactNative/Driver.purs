module Halogen.ReactNative.Driver  where

import Prelude

import Data.Foldable (foldMap)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Component
import Halogen.Query.Input (Input)
import Halogen.ReactNative.Core (VIEW(..), Native(..), runGraft)
import Halogen.ReactNative.Unsafe.Elements (textElemU, textU)
import ReactNative.Basic (NativeClass, NativeElement, NativeProps, NativeThis, Prop(..))
import ReactNative.Basic as RB


newtype RenderState state action (slots :: # Type) output =
  RenderState
    { keyId :: Int
    , rclass :: NativeClass
    , self :: NativeThis
    , node :: Maybe NativeElement
    }

type AppName = String


runUI
  :: forall query input output
   . Component VIEW query input output Aff
  -> input
  -> AppName
  -> Aff (HalogenIO query output Aff)
runUI component input appName = do
  keyId <- liftEffect (Ref.new 0)
  AD.runUI (mkRenderSpec appName keyId) component input

mkRenderSpec
  :: AppName
  -> Ref Int
  -> AD.RenderSpec VIEW RenderState
mkRenderSpec appName keyRef =
  { render
  , renderChild: identity
  , removeChild: const (pure unit)
  , dispose: const (pure unit)
  }
  where

  render
    :: forall state action slots output
     . (Input action -> Effect Unit)
    -> (ComponentSlotBox VIEW slots Aff action -> Effect (RenderStateX RenderState))
    -> VIEW (ComponentSlot VIEW slots Aff action) action
    -> Boolean
    -> Maybe (RenderState state action slots output)
    -> Effect (RenderState state action slots output)
  render handler renderChild view isRoot lastRender = do
    node <- renderView handler (map getElement <<< renderChild) view
    case lastRender of
      Nothing -> do
        keyId <- Ref.modify (_ + 1) keyRef
        let {rclass, self} = RB.mkComponent node
        RB.registerComponent appName rclass
        pure $ RenderState {keyId, rclass, self, node: Nothing}
      Just (RenderState r) -> do
        RB.updateState node r.self
        pure $ RenderState r

getElement :: RenderStateX RenderState -> NativeElement
getElement = unRenderStateX \(RenderState { node, rclass }) ->
  case node of
    Just n -> n
    Nothing -> RB.element rclass mempty

renderView
  :: forall slots action
   . (Input action -> Effect Unit)
  -> (ComponentSlotBox VIEW slots Aff action -> Aff NativeElement)
  -> VIEW (ComponentSlot VIEW slots Aff action) action
  -> Aff NativeElement
renderView handler handleWidget (VIEW view) = go view
  where
    go :: Native (Array (Prop (Input action))) (ComponentSlot VIEW slots Aff action) -> Aff NativeElement
    go (Text str) =
      let textElem = textElemU str
          props = runFn2 RB.prop "children" textElem
        in pure $ RB.element textU props

    go (Elem nClass props children) = do
      children' <- traverse go children
      let childProp = runFn2 RB.prop "children" children'
      let nativeProps = (foldMap (renderProp handler) props <> childProp)
      pure $ RB.element nClass nativeProps

    go (Grafted gd) = go (runGraft gd)

    go (Widget (slot :: ComponentSlot VIEW slots Aff action)) =
      case slot of
        ComponentSlot cs -> unsafeCoerce unit
          -- | EFn.runEffectFn3 renderComponentSlot renderChildRef render cs
        ThunkSlot t -> do
          step <- EFn.runEffectFn1 (Thunk.buildThunk unwrap spec) t
          pure $ V.mkStep $ V.Step (V.extract step) (Just step) (Fn.runFn2 mkPatch renderChildRef render) widgetDone

      -- ?a $ handleWidget w

renderProp
  :: forall action
   . (Input action -> Effect Unit)
  -> Prop (Input action)
  -> NativeProps
renderProp handler = case _ of
  Property name value -> runFn2 RB.prop name value
  Handler evType k ->
    runFn2 RB.handlerProp evType (maybe (pure unit) handler <<< k)
  Nested name prop -> runFn2 RB.prop name (renderProp handler prop)
