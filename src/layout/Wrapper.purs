module Wrapper (wrapper, WrapperProps, ChildStyles) where

import Prelude
import Data.Array (mapWithIndex, length, cons)
import React.Basic (Component, JSX, StateUpdate(..), createComponent, make)
import React.Basic.DOM (CSS, css, mergeStyles)
import React.Basic.DOM as R

component :: Component WrapperProps
component = createComponent "Wrapper"

type ChildStyles = 
  { first :: CSS
  , middle :: CSS
  , last :: CSS
  }

type WrapperProps =
  { style :: CSS
  , children :: Array JSX
  , wrapperStyle :: CSS
  , childStyle :: CSS
  , posStyles :: ChildStyles
  }

wrapper :: WrapperProps -> JSX
wrapper = make component { initialState, update, render }
  where
    initialState = unit

    update self _ = Update self.state

    render self =
        R.div { style: self.props.wrapperStyle
              , children: wrapChildren self.props self.props.children
              }

wrapChildren :: WrapperProps -> Array JSX -> Array JSX
wrapChildren props as = mapWithIndex (addStyle props (length as)) as

addStyle :: WrapperProps -> Length -> Index -> JSX -> JSX
addStyle props length i a = 
  R.div { style: wrapStyle
        , children: [a] 
        } where
          wrapStyle = getPositionedStyle props.posStyles length i props.childStyle

getPositionedStyle :: ChildStyles -> Length -> Index -> CSS -> CSS
getPositionedStyle childStyles l i style = mergeStyles (cons style (applicableStyles childStyles l i))

applicableStyles :: ChildStyles -> Length -> Index -> Array CSS
applicableStyles childStyles l i = [
  if isFirst l i then childStyles.first else css {}
, if isMiddle l i then childStyles.middle else css {}
, if isLast l i then childStyles.last else css {}
]

type Length = Int
type Index = Int

isFirst :: Length -> Index -> Boolean
isFirst _ i = i == 0

isLast :: Length -> Index -> Boolean
isLast l i = i == (l - 1)

isMiddle :: Length -> Index -> Boolean
isMiddle l i = not isFirst l i && not isLast l i
