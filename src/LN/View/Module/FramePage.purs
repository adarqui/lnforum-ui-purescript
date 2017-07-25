module LN.View.Module.FramePage (
  renderFramePage
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P

import LN.Input.Types                  (Input)



renderFramePage :: ComponentHTML Input
renderFramePage =
  H.div_ [
    H.iframe [P.src "http://dev.stephendiehl.com/hask/#flags"]
  ]
