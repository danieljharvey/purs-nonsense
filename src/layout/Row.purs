module Row (row) where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM (CSS, css)
import Type.Data.Boolean (kind Boolean)
import Wrapper (wrapper, WrapperProps)

type Spacing = Int

type RowProps =
  { style :: CSS
  , children :: Array JSX
  , spacing :: Spacing
  }

addRowStyle :: RowProps -> WrapperProps
addRowStyle r = { wrapperStyle: rowStyle
                , style: r.style
                , children: r.children
                , childStyle: childStyle
                , posStyles: { first: firstStyle r.spacing
                             , middle: middleStyle r.spacing
                             , last: lastStyle r.spacing
                             }                          
                }

childStyle :: CSS
childStyle = css { display: "flex" }

rowStyle :: CSS
rowStyle = css { flex: 1, flexDirection: "row", display: "flex" }

row :: RowProps -> JSX
row props = wrapper (addRowStyle props)


firstStyle :: Spacing -> CSS
firstStyle s = css { color: "red", margin: "0 0 0 " <> show s <> "px" }

middleStyle :: Spacing -> CSS
middleStyle s = css { color: "orange", margin: "0 0 0 " <> show s <> "px" }

lastStyle :: Spacing -> CSS
lastStyle s = css { color: "green", margin: "0 " <> show s <> "px 0 " <> show s <> "px" }
