module Form.TextBox where

import Prelude (($), (>>>))
import Data.Maybe (fromMaybe, Maybe(..))
import React.Basic
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import Type.Data.Boolean (kind Boolean)
import InputType (FormInput)
import Interaction (InteractionState, InteractionMode(..), defaultInteraction, click, blur, focus, change)

type InputProps e = {
  formInput :: FormInput e String
}

type InputState =
  { interactionState :: InteractionState
  , value :: Maybe String
  }

data InputAction = Blur
                 | Click
                 | Focus
                 | Change String

type ThisGuy e = Self (InputProps e) InputState InputAction

component :: forall p. Component p
component = createComponent "Input"

input :: forall e. InputProps e -> JSX
input = make component { initialState, update, render }
  where
    initialState =
      { interactionState: defaultInteraction Always
      , value: Nothing
      }

    update self = case _ of
      Blur -> Update $ self.state { interactionState = blur self.state.interactionState }
      Click -> Update $ self.state { interactionState = click self.state.interactionState }
      Focus -> Update $ self.state { interactionState = focus self.state.interactionState }
      Change str -> Update $ self.state { interactionState = change self.state.interactionState, value = Just str }

    render self =
       R.input { type: "text"
               , value: fromMaybe self.props.formInput.initial self.state.value
               , onBlur: capture_ self $ Blur
               , onClick: capture_ self $ Click
               , onFocus: capture_ self $ Focus
               , onChange: capture self (preventDefault >>> targetValue) (\a -> Change $ (fromMaybe self.props.formInput.initial a))
               }
