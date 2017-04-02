module LN.View.Resources.SiftLeuronsRandom (
  renderView_Resources_SiftLeuronsRandom
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.HTML.Events     as E
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (($))

import LN.Input.Types                  (Input(..))
import LN.State.Loading                (getLoading, l_currentLeuron)
import LN.State.Types                  (State)
import LN.View.Leurons.Show            (renderView_Leurons_Show')
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( LeuronPackResponse
                                       , _LeuronPackResponse, _LeuronResponse
                                       , leuron_)



renderView_Resources_SiftLeuronsRandom :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsRandom resource_id st =
  case st.currentLeuron, getLoading l_currentLeuron st.loading of
    _, true          -> renderLoading
    Nothing, false   -> H.div_ [H.p_ [H.text "no leurons."]]
    Just pack, false -> renderView_Resources_SiftLeuronsRandom' pack st



renderView_Resources_SiftLeuronsRandom' :: LeuronPackResponse -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsRandom' pack st =
  H.div_ [
    renderButtons pack st,
    H.div [P.class_ B.preScrollable] [renderView_Leurons_Show' pack st]
  ]



renderButtons :: LeuronPackResponse -> State -> ComponentHTML Input
renderButtons pack st =
  H.div_ [
    H.button [
      P.classes [B.btnInfo, B.btnLg],
      E.onClick $ E.input_ $ GetResourceLeuronRandom leuron.resourceId
    ] [H.text "next"]
  ]
  where
  leuron = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
