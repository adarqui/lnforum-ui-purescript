module LN.View.Four04 (
  renderView_404
) where



import Halogen              (ComponentHTML)
import Halogen.HTML as H

import LN.Input.Types       (Input)


renderView_404 :: ComponentHTML Input
renderView_404 =
  H.div_ [
    H.p_ [ H.text "404" ]
  ]
