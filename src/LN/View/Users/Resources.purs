module LN.View.Users.Resources (
  renderView_Users_Resources
) where



import Halogen              (ComponentHTML)
import Halogen.HTML as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)
import LN.View.Users        (usersLayout)



renderView_Users_Resources :: String -> State -> ComponentHTML Input
renderView_Users_Resources user_name st =
  usersLayout user_name st [
    H.div_ [
      H.h1_ [ H.text "Resources." ]
    ]
  ]
