module AsyncCounter where

import Prelude

import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import React.Basic (Component, JSX, ReactComponentInstance, StateUpdate(..), createComponent, fragment, keyed, make, send)
import React.Basic.Components.Async (asyncWithLoader)
import React.Basic.DOM as R
import React.Basic.Events as RE
import AsyncActionTypes (AsyncAction(..))
import TotalButton (totalButton)

component :: Component Props
component = createComponent "AsyncCounter"

type Props =
  { label :: String
  }

type ComponentSelf = { props :: { label :: String }
                     , state :: { counter :: Int }
                     , instance_ :: ReactComponentInstance }

asyncCounter :: Props -> JSX
asyncCounter = make component { initialState, update, render }
  where
    initialState = { counter: 0 }

    update self = case _ of
      Increment ->
        Update self.state { counter = self.state.counter + 1 }

    render = doRender
      
doRender :: ComponentSelf -> JSX
doRender self = fragment
  [ R.p_ [ R.text "Notes:" ]
  , R.ol_
      [ R.li_ [ R.text "The two counts should never be out of sync" ]
      , R.li_ [ R.text "\"done\" should only be logged to the console once for any loading period (in-flight requests get cancelled as the next request starts)" ]
      ]
  , totalButton { buttonLabel: self.props.label
                , counter: self.state.counter
                , onChange: RE.handler_ (send self Increment)
                }
  , keyed (show self.state.counter) $
      asyncWithLoader (R.text "Loading...") do
        liftEffect $ log "start"
        delay $ Milliseconds 2000.0
        liftEffect $ log "done"
        pure $ R.text $ "Done: " <> show self.state.counter
  ]


