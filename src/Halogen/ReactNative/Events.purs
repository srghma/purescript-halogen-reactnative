module Halogen.ReactNative.Events
  ( handler
  , touchHandler
  , pressHandler
  , changeTextHandler
  , keyPressHandler
  , scrollHandler
  , selectionChangeHandler
  , onPress
  , onResponderMove
  , onChangeText
  , onSelectionChange
  , onSubmitEditing
  , onKeyPress
  ) where


import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Query.Input (Input(..))
import Halogen.ReactNative.Properties (IProp(..))
import ReactNative.Basic (EventType(..), NativeEvent, Prop(..))
import ReactNative.EventTypes (PressEvent, ScrollEvent, TextInputEvent, TouchEvent, SelectionChangeEvent, KeyPressEvent)
import Unsafe.Coerce (unsafeCoerce)

handler :: forall r i. EventType -> (NativeEvent -> i) -> IProp r i
handler et f = IProp $ Handler et \ev -> Just (Action (f ev))


pressHandler :: forall i . (PressEvent -> i) -> (NativeEvent -> i)
pressHandler = unsafeCoerce

touchHandler :: forall i . (TouchEvent -> i) -> (NativeEvent -> i)
touchHandler = unsafeCoerce

changeTextHandler :: forall i. (String -> i) -> (NativeEvent -> i)
changeTextHandler = unsafeCoerce

keyPressHandler :: forall i . (KeyPressEvent -> i) -> (NativeEvent -> i)
keyPressHandler = unsafeCoerce

scrollHandler :: forall i . (ScrollEvent -> i) -> (NativeEvent -> i)
scrollHandler = unsafeCoerce

selectionChangeHandler :: forall i . (SelectionChangeEvent -> i) -> (NativeEvent -> i)
selectionChangeHandler = unsafeCoerce



onPress :: forall r i . (PressEvent -> i) -> IProp (onPress :: PressEvent | r) i
onPress = handler (EventType "onPress") <<< pressHandler

onResponderGrant :: forall r i . (TouchEvent -> i) -> IProp (onResponderGrant :: TouchEvent | r) i
onResponderGrant = handler (EventType "onResponderGrant") <<< touchHandler

onResponderMove :: forall r i . (TouchEvent -> i) -> IProp (onResponderMove :: TouchEvent | r) i
onResponderMove = handler (EventType "onResponderMove") <<< touchHandler

onResponderReject :: forall r i . (TouchEvent -> i) -> IProp (onResponderReject :: TouchEvent | r) i
onResponderReject = handler (EventType "onResponderReject") <<< touchHandler

onResponderRelease :: forall r i . (TouchEvent -> i) -> IProp (onResponderRelease :: TouchEvent | r) i
onResponderRelease = handler (EventType "onResponderRelease") <<< touchHandler

onResponderTerminate :: forall r i . (TouchEvent -> i) -> IProp (onResponderTerminate :: TouchEvent | r) i
onResponderTerminate = handler (EventType "onResponderTerminate") <<< touchHandler

onResponderTerminationRequest
  :: forall r i
   . (TouchEvent -> i)
  -> IProp (onResponderTerminationRequest :: TouchEvent | r) i
onResponderTerminationRequest =
  handler (EventType "onResponderTerminationRequest") <<< touchHandler

onChangeText :: forall r i . (TextInputEvent -> i) -> IProp (onChangeText :: TextInputEvent | r) i
onChangeText = handler (EventType "onChangeText") <<< changeTextHandler

onScroll :: forall r i . (ScrollEvent -> i) -> IProp (onScroll :: ScrollEvent | r) i
onScroll = handler (EventType "onScroll") <<< scrollHandler

onSelectionChange
  :: forall r i
   . (SelectionChangeEvent -> i)
  -> IProp (onSelectionChange :: SelectionChangeEvent | r) i
onSelectionChange = handler (EventType "onSelectionChange") <<< selectionChangeHandler

onSubmitEditing :: forall r i . (Unit -> i) -> IProp (onSubmitEditing :: Unit | r) i
onSubmitEditing = handler (EventType "onSubmitEditing") <<< pressHandler

onKeyPress :: forall r i . (KeyPressEvent -> i) -> IProp (onKeyPress :: KeyPressEvent | r) i
onKeyPress = handler (EventType "onKeyPress") <<< keyPressHandler

