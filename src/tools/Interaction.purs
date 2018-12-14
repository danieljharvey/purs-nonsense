module Interaction where

import Prelude (not)

type InteractionState = {
    focused      :: Boolean
  , hasFocused   :: Boolean
  , hasBlurred   :: Boolean
  , hasClicked   :: Boolean
  , hasChanged   :: Boolean
  , hasSubmitted :: Boolean
  , mode         :: InteractionMode
}

data InteractionMode = Always
                     | AfterChange
                     | AfterFocus
                     | AfterBlurred
                     | AfterClicked
                     | AfterSubmit
                     | NotFocused

defaultInteraction :: InteractionMode -> InteractionState
defaultInteraction mode =
  {
    focused:      false
  , hasFocused:   false
  , hasBlurred:   false
  , hasClicked:   false
  , hasChanged:   false
  , hasSubmitted: false
  , mode
  }

showValidation :: InteractionState -> Boolean
showValidation state = case state.mode of
  Always       -> true
  AfterChange  -> state.hasChanged
  AfterFocus   -> state.hasFocused
  AfterBlurred -> state.hasBlurred
  AfterClicked -> state.hasClicked
  AfterSubmit  -> state.hasSubmitted
  NotFocused   -> not state.focused

focus :: InteractionState -> InteractionState
focus is = is { hasFocused = true, focused = true }

blur :: InteractionState -> InteractionState
blur is = is { hasBlurred = true, focused = false }

click :: InteractionState -> InteractionState
click is = is { hasClicked = true }

change :: InteractionState -> InteractionState
change is = is { hasChanged = true }
