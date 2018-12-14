module Form (form) where

import Data.Array (index)
import Data.Eq (class Eq)
import Data.Maybe (fromMaybe)
import Prelude (map, show, ($), (<$>), (==), (>>>))
import React.Basic (Component, JSX, StateUpdate(..), capture_, capture, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import Type.Data.Boolean (kind Boolean)
import Column (column)

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

type State =
  {
    fields :: Array (FormItem Key)
  }

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
      { fields: [ { key: FirstName
                , inputType: TextBox ""
                , interactionState: defaultInteraction AfterChange
                }
              , { key: LastName
                , inputType: TextBox ""
                , interactionState: defaultInteraction AfterBlurred
                }
              ]
     }

    update self = case _ of
      Blur key -> Update self.state { fields = updateInteraction key blur self.state.fields }
      Click key -> Update self.state { fields = updateInteraction key click self.state.fields }
      Focus key -> Update self.state { fields = updateInteraction key focus self.state.fields }
      Change key _ -> Update self.state { fields = updateInteraction key change self.state.fields }

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
                               , R.text $ show $ fromMaybe false (showValidation <$> (\a -> a.interactionState) <$> index self.state.fields 0)
                               , R.text self.props.title
                               , R.text self.props.title
                               , R.text self.props.title
                               , R.input { type: "text"
                                         , onBlur: capture_ self $ Blur LastName
                                         , onClick: capture_ self $ Click LastName
                                         , onFocus: capture_ self $ Focus LastName
                                         , onChange: capture self (preventDefault >>> targetValue) (\a -> Change LastName $ TextBox (fromMaybe "" a))
                                         }
                               , R.text "Blurred?"
                               , R.text $ show $ fromMaybe false (showValidation <$> (\a -> a.interactionState) <$> index self.state.fields 1)
                               , R.text self.props.title
                               ]
                    , style : R.css { background: "lightgray" }
                    , spacing: 30
                   }
                ]
