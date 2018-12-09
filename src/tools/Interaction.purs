module Interaction where
  
import Prelude (not)

type InteractionState = {
    focused :: Boolean
  , hasFocused :: Boolean
  , hasBlurred :: Boolean
  , hasClicked :: Boolean
  , hasChanged :: Boolean
}

data InteractionMode = Always
                     | AfterChange
                     | AfterFocus
                     | AfterBlurred
                     | AfterClicked
                     | NotFocused

showValidation :: InteractionMode -> InteractionState -> Boolean
showValidation Always _ = true
showValidation AfterChange state = state.hasChanged
showValidation AfterFocus state = state.hasFocused
showValidation AfterBlurred state = state.hasBlurred
showValidation AfterClicked state = state.hasClicked
showValidation NotFocused state = not state.focused