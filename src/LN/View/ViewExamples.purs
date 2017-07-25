module LN.View.ViewExamples (
  renderView_ViewExamples
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Prelude (map, ($))

import LN.Input.Types                  (Input)
import LN.Router.Types (Routes(Home))
import LN.Router.Link                  (linkToP_Classes)



renderView_ViewExamples :: ComponentHTML Input
renderView_ViewExamples = H.div_ $ map (\v -> H.div [P.class_ B.row] [v]) [sixButtons1, sixButtons2, buttonToolbar1, buttonToolbar2]



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



buttonToolbar2 :: ComponentHTML Input
buttonToolbar2 =
  H.div [P.class_ B.containerFluid] [
    H.h4_ [H.text "buttonToolbar2"],
    H.div [P.class_ B.btnToolbar] [
      H.div [P.class_ B.btnGroup] [
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo] [] Home "PREV",
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo] [] Home "NEXT",
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo] [] Home "KNOW",
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo] [] Home "?",
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo] [] Home "~CARE",
        linkToP_Classes [B.btn, B.btnSm, B.btnInfo] [] Home "FLAG"
      ]
    ]
  ]
