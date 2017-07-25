module LN.View.Home (
  renderView_Home
) where



import Halogen                (ComponentHTML)
import Halogen.HTML   as H

import LN.Input.Types         (Input)



renderView_Home :: ComponentHTML Input
renderView_Home =
  H.div_ [
   H.h1_ [ H.text "Home" ],
   H.p_ [
   ]
  ]
