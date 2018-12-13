module Column (column) where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM (CSS, css)
import Type.Data.Boolean (kind Boolean)
import Wrapper (wrapper, WrapperProps)

type Spacing = Int

type ColumnProps =
  { style :: CSS
  , children :: Array JSX
  , spacing :: Spacing
  }

addColumnStyle :: ColumnProps -> WrapperProps
addColumnStyle col = { wrapperStyle: columnStyle
                     , style: col.style
                     , children: col.children
                     , childStyle: childStyle
                     , posStyles: { first: firstStyle col.spacing
                                  , middle: middleStyle col.spacing
                                  , last: lastStyle col.spacing
                                  }                          
                     }

childStyle :: CSS
childStyle = css { display: "flex" }

columnStyle :: CSS
columnStyle = css { flex: 1, flexDirection: "column", display: "flex" }

column :: ColumnProps -> JSX
column props = wrapper (addColumnStyle props)

firstStyle :: Spacing -> CSS
firstStyle s = css { color: "red", margin: show s <> "px 0 0 0" }

middleStyle :: Spacing -> CSS
middleStyle s = css { color: "orange", margin: show s <> "px 0 0 0" }

lastStyle :: Spacing -> CSS
lastStyle s = css { color: "green", margin: show s <> "px 0 " <> show s <> "px 0" }
