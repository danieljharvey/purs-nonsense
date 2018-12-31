module Form (form) where

import Data.Array (index)
import Data.Eq (class Eq)
import Data.Ord ((<))
import Data.String (length)
import Data.Maybe (fromMaybe)
import Effect.Console (log)
import Prelude (map, show, ($), (<$>), (==), (>>>), discard, (<>), pure, unit)
import React.Basic (Component, JSX, StateUpdate(..), capture_, capture, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import Type.Data.Boolean (kind Boolean)
import Column (column)
import Form.TextBox (input)

import InputType (FormInput)
import Validators (makeValidator)
import Interaction

component :: Component FormProps
component = createComponent "Form"

type FormProps =
  { title :: String
  }

data FormAction = Blur Key | Click Key | Focus Key | Change Key InputType

data InputType = TextBox String

type FormItem a =
  {
    key              :: a
  , inputType        :: InputType
  , interactionState :: InteractionState
  }

data Key = FirstName | LastName
derive instance eqKey :: Eq Key

type State = Array (FormItem Key)

updateAll :: Key -> (FormItem Key -> FormItem Key) -> Array (FormItem Key) -> Array (FormItem Key)
updateAll key f as = map (mapFields key f) as

mapFields :: Key -> (FormItem Key -> FormItem Key) -> FormItem Key -> FormItem Key
mapFields key f a = if a.key == key then f a else a

updateInteraction :: Key -> (InteractionState -> InteractionState) -> Array (FormItem Key) -> Array (FormItem Key)
updateInteraction key f as = updateAll key (\item -> item { interactionState = f item.interactionState } ) as

form :: FormProps -> JSX
form = make component { initialState, update, render }
  where
    initialState =
      [ { key: FirstName
        , inputType: TextBox ""
        , interactionState: defaultInteraction AfterChange
        }
      , { key: LastName
        , inputType: TextBox ""
        , interactionState: defaultInteraction AfterBlurred
        }
      ]

    update self = case _ of
      Blur key -> Update $ updateInteraction key blur self.state
      Click key -> Update $ updateInteraction key click self.state
      Focus key -> Update $ updateInteraction key focus self.state
      Change key _ -> Update $ updateInteraction key change self.state

    render self =
        R.form_ [
            column { children: [ R.text self.props.title
                               , R.input { type: "text"
                                         , onBlur: capture_ self $ Blur FirstName
                                         , onClick: capture_ self $ Click FirstName
                                         , onFocus: capture_ self $ Focus FirstName
                                         , onChange: capture self (preventDefault >>> targetValue) (\a -> Change FirstName $ TextBox (fromMaybe "" a))
                                         }
                               , R.text "Changed?"
                               , R.text $ show $ fromMaybe false (showValidation <$> (\a -> a.interactionState) <$> index self.state 0)
                               , input { formInput: textBox }
                               , R.text "Blurred?"
                               , R.text $ show $ fromMaybe false (showValidation <$> (\a -> a.interactionState) <$> index self.state 1)
                               ]
                    , style : R.css { background: "lightgray" }
                    , spacing: 30
                   }
                ]



data TextBoxError = Empty | TooLong

textBox :: FormInput TextBoxError String
textBox =
  { initial: "", validate: textBoxValidate, update: textBoxUpdate } where

    textBoxValidate = makeValidator (\a -> length a < 1) Empty

    textBoxUpdate formState = do
      log $ "Focused? " <> show formState.interactionState.focused
      log $ "Has been focused? " <> show formState.interactionState.hasFocused
      log $ "Has been blurred? " <> show formState.interactionState.hasBlurred
      log $ "Has been clicked? " <> show formState.interactionState.hasClicked
      log $ "Has been changed? " <> show formState.interactionState.hasChanged
      log $ "Has been submitted? " <> show formState.interactionState.hasSubmitted
      pure unit
