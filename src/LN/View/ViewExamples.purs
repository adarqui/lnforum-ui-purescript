module LN.View.ViewExamples (
  renderView_ViewExamples
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (map, bind, (==), (+), (-), ($))

import LN.Input.Types                  (Input)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentLeuron)
import LN.State.Types                  (State)
import LN.View.Leurons.Show            (renderView_Leurons_Show')
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( LeuronPackResponse
                                       , _LeuronPackResponse, _LeuronResponse
                                       , leuron_)



renderView_ViewExamples :: ComponentHTML Input
renderView_ViewExamples = H.div_ $ map (\v -> H.div [P.class_ B.row] [v]) [sixButtons1, sixButtons2, buttonToolbar1]



sixButtons1 :: ComponentHTML Input
sixButtons1 =
  H.div [P.class_ B.containerFluid] [
    H.div_ [H.h4_ [H.text "sixButtons1"]],
    H.div [P.class_ B.row] [
      H.div [P.classes [B.colLg2, B.colMd2, B.colXs2]] [
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo, B.btnBlock] [] Home "PREV"
      ],
      H.div [P.classes [B.colLg2, B.colMd2, B.colXs2]] [
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo, B.btnBlock] [] Home "NEXT"
      ],
      H.div [P.classes [B.colLg2, B.colMd2, B.colXs2]] [
        linkToP_Classes [B.btn, B.btnSm, B.btnSuccess, B.btnBlock] [] Home "KNOW"
      ],
      H.div [P.classes [B.colLg2, B.colMd2, B.colXs2]] [
        linkToP_Classes [B.btn, B.btnSm, B.btnWarning, B.btnBlock] [] Home "?"
      ],
      H.div [P.classes [B.colLg2, B.colMd2, B.colXs2]] [
        linkToP_Classes [B.btn, B.btnSm, B.btnDanger, B.btnBlock] [] Home "~CARE"
      ],
      H.div [P.classes [B.colLg2, B.colMd2, B.colXs2]] [
        linkToP_Classes [B.btn, B.btnSm, B.btnDanger, B.btnBlock] [] Home "FLAG"
      ]
    ]
  ]



sixButtons2 :: ComponentHTML Input
sixButtons2 =
  H.div [P.class_ B.containerFluid] [
    H.h4_ [H.text "sixButtons2"],
    H.div [P.class_ B.row] [
      H.div [P.class_ B.colXs12] [
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo, B.btnBlock] [] Home "PREV",
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo, B.btnBlock] [] Home "NEXT",
        linkToP_Classes [B.btn, B.btnSm, B.btnSuccess, B.btnBlock] [] Home "KNOW",
        linkToP_Classes [B.btn, B.btnSm, B.btnWarning, B.btnBlock] [] Home "?",
        linkToP_Classes [B.btn, B.btnSm, B.btnDanger, B.btnBlock] [] Home "~CARE",
        linkToP_Classes [B.btn, B.btnSm, B.btnDanger, B.btnBlock] [] Home "FLAG"
      ]
    ]
  ]



buttonToolbar1 :: ComponentHTML Input
buttonToolbar1 =
  H.div [P.class_ B.containerFluid] [
    H.h4_ [H.text "buttonToolbar1"],
    H.div [P.class_ B.btnToolbar] [
      H.div [P.class_ B.btnGroup] [
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "one"],
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "two"],
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "three"],
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "four"]
      ],
      H.div [P.class_ B.btnGroup] [
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "five"],
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "six"],
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "seven"]
      ],
      H.div [P.class_ B.btnGroup] [
        H.button [P.classes [B.btn, B.btnPrimary]] [H.text "eight"]
      ]
    ]
  ]
