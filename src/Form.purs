module Form (form) where

import Data.Maybe (fromMaybe)
import Prelude (mempty, show, (<>), (>>>))
import React.Basic (Component, JSX, StateUpdate(..), capture_, capture, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import Type.Data.Boolean (kind Boolean)

component :: Component FormProps
component = createComponent "Form"

type FormProps =
  { title :: String
  }

data FormAction = Blur | Click | Focus | Change String

type State = {
    focused :: Boolean
  , hasFocused :: Boolean
  , hasBlurred :: Boolean
  , hasClicked :: Boolean
  , hasChanged :: Boolean
  , value :: String
}

type InteractionState = {
    focused :: Boolean
  , hasFocused :: Boolean
  , hasBlurred :: Boolean
  , hasClicked :: Boolean
  , hasChanged :: Boolean
}

form :: FormProps -> JSX
form = make component { initialState, update, render }
  where
    initialState = { focused : false
                   , hasFocused : false
                   , hasBlurred : false
                   , hasClicked : false
                   , hasChanged : false
                   , value : mempty
                   }

    update self = case _ of
      Blur ->
        Update self.state { hasBlurred = true, focused = false }
      Click ->
        Update self.state { hasClicked = true }
      Focus ->
        Update self.state { hasFocused = true, focused = true }
      (Change s) ->
        Update self.state { hasChanged = true, value = s }

    render self =
        R.form_ [
            R.div_ [ R.text self.props.title 
                   , R.input { type: "text"
                             , onBlur: capture_ self Blur
                             , onClick: capture_ self Click
                             , onFocus: capture_ self Focus
                             , onChange: capture self (preventDefault >>> targetValue) (\a -> Change (fromMaybe "" a))
                             }
                   ], showState self.state
        ]


niceLabel :: String -> JSX
niceLabel s = R.p_ [ R.text s ]

showState :: State -> JSX
showState state = R.div_ [
  niceLabel ("Value: " <> state.value)
, niceLabel ("Focused " <> show state.focused)
, niceLabel ("Has focused " <> show state.hasFocused)
, niceLabel ("Has blurred " <> show state.hasBlurred)
, niceLabel ("Has clicked " <> show state.hasClicked)
, niceLabel ("Has changed " <> show state.hasChanged)
]
