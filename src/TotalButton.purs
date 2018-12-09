module TotalButton (totalButton) where

import Prelude

import Data.Int (even)
import Data.Tuple (Tuple(..))
import React.Basic (Component, JSX, StateUpdate(..), createComponent, make)
import React.Basic.DOM (style)
import React.Basic.DOM as R
import React.Basic.Events as RE

component :: Component ButtonProps
component = createComponent "TotalButton"

type ButtonProps =
  { buttonLabel :: String
  , counter :: Int
  , onChange :: RE.EventHandler
  }


totalButtonStyles :: ButtonProps -> R.CSS
totalButtonStyles props = case even props.counter of
    true -> R.css { color: "red", padding: "15px" }
    false -> R.css { color: "blue", padding: "20px" }

totalButton :: ButtonProps -> JSX
totalButton = make component { initialState, update, render }
  where
    initialState = { counter: 0 }

    update self = case _ of
      _ ->
        Update self.state

    render self =
     R.button
            { onClick: self.props.onChange
            , className: "totalButton"
            , style: totalButtonStyles self.props
            , children: [ R.text (self.props.buttonLabel <> ": " <> show self.props.counter) ]
            }
        