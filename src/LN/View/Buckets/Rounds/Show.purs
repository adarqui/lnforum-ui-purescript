module LN.View.Buckets.Rounds.Show (
  renderView_Buckets_Rounds_Show,
  renderView_Buckets_Rounds_Show'
) where



import Data.Maybe                      (Maybe(..))
import CSS                             as CSS
import Halogen.HTML.CSS        as HCSS
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, map, ($), (<>))

import LN.Input.BucketRound
import LN.Input.Types                  (Input, cBucketRound)
import LN.Router.Link                  (linkTo, linkToP, linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentLeuron)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Leurons.Show            (renderView_Leurons_Show, renderLeuron)
import LN.T                            ( LeuronPackResponse, LeuronResponse
                                       , LeuronResponseR
                                       , LeuronData(..)
                                       , unwrapFact, unwrapFactList, unwrapCard, unwrapDCard, unwrapDCardX
                                       , unwrapAcronym, unwrapSynonym, unwrapAntonym, unwrapTemplate
                                       , unwrapImageAssociation, unwrapLinearDemo, unwrapScript, unwrapQA
                                       , unwrapTable
                                       , FactR, FactListR, CardR, DCardR, DCardXR
                                       , AcronymR, SynonymR, AntonymR, TemplateR
                                       , ImageAssociationR, LinearDemoR, ScriptR, QAR
                                       , TableR
                                       , _LeuronPackResponse, _LeuronResponse
                                       , leuron_)



renderView_Buckets_Rounds_Show :: State -> ComponentHTML Input
renderView_Buckets_Rounds_Show st =

  case st.currentLeuron, getLoading l_currentLeuron st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.p_ [H.text "leuron unavailable."]
       Just pack, false -> renderView_Buckets_Rounds_Show' pack st



renderView_Buckets_Rounds_Show' :: LeuronPackResponse -> State -> ComponentHTML Input
renderView_Buckets_Rounds_Show' pack st =

  H.div [P.class_ B.containerFluid] [
    H.div_ [H.p_ [H.text $ "Remaining: " <> show st.currentBucketRoundLeuronsCount]],
    H.div [P.class_ B.row] [
      H.div [P.classes [B.colLg3, B.colMd3, B.colXs3]] [
        H.button [P.classes [B.btn, B.btnSm, B.btnSuccess, B.btnBlock],
          E.onClick (E.input_ $ cBucketRound $ InputBucketRound_Op leuron.id "know")
        ] [H.text "KNOW"]
      ],
      H.div [P.classes [B.colLg3, B.colMd3, B.colXs3]] [
        H.button [P.classes [B.btn, B.btnSm, B.btnWarning, B.btnBlock],
          E.onClick (E.input_ $ cBucketRound $ InputBucketRound_Op leuron.id "dont_know")
        ] [H.text "DONT-KNOW"]
      ],
      H.div [P.classes [B.colLg3, B.colMd3, B.colXs3]] [
        H.button [P.classes [B.btn, B.btnSm, B.btnDanger, B.btnBlock],
          E.onClick (E.input_ $ cBucketRound $ InputBucketRound_Op leuron.id "dont_care")
        ] [H.text "DONT-CARE"]
      ],
      H.div [P.classes [B.colLg3, B.colMd3, B.colXs3]] [
        H.button [P.classes [B.btn, B.btnSm, B.btnDanger, B.btnBlock],
          E.onClick (E.input_ $ cBucketRound $ InputBucketRound_Op leuron.id "flag")
        ] [H.text "FLAG"]
      ]
    ],

    H.div [P.class_ B.container] [
      renderLeuron leuron'
    ]
  ]

 where
 leuron  = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
 leuron' = pack ^. _LeuronPackResponse .. leuron_
