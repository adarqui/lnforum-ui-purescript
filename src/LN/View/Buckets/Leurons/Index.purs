module LN.View.Buckets.Leurons.Index (
  renderView_Buckets_Leurons_Index
) where



import Data.Map                        as Map
import Data.Maybe
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, map, not, ($), (<>))

import LN.ArrayList           (listToArray)
import LN.Internal.Leuron              (leuronToTyLeuron)
import LN.Input.Types                  (Input, cBucketMod)
import LN.Input.Bucket
import LN.Router.Link                  (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.View.Leurons.Show
import LN.View.Buckets.Nav
import LN.T                            ( Size(Small)
                                       , _LeuronStatResponse, _LeuronPackResponse, _LeuronResponse
                                       , stat_, leuron_
                                       , BucketPackResponse)



renderView_Buckets_Leurons_Index :: Int -> State -> ComponentHTML Input
renderView_Buckets_Leurons_Index bucket_id st =
  case st.resources, st.currentBucket, getLoading l_currentBucket st.loading of
       _, _, true -> renderLoading
       _, Nothing, false -> H.div_ [H.p_ [H.text "bucket unavailable."]]
       _, Just pack, false -> renderView_Buckets_Leurons_Index' bucket_id pack st



renderView_Buckets_Leurons_Index' :: Int -> BucketPackResponse -> State -> ComponentHTML Input
renderView_Buckets_Leurons_Index' bucket_id pack st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Leurons"]
    ],

    renderView_Buckets_Nav pack st,

    -- Leurons
    H.div [] [renderLeurons bucket_id st]
  ]



renderLeurons :: Int -> State -> ComponentHTML Input
renderLeurons bucket_id st =
  H.div_ [
    renderPageNumbers st.leuronsPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\pack ->
          let
            leuron_pack = pack ^. _LeuronPackResponse
            leuron      = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
            stat        = pack ^. _LeuronPackResponse .. stat_ ^. _LeuronStatResponse
            member      = Map.member leuron.id m
          in
          H.li_ [
            H.div [P.class_ B.row] [
                H.div [P.class_ B.colSm1] [H.input [P.type_ P.InputCheckbox, P.name "select-leuron",
                                           E.onChecked (E.input_ (cBucketMod $ SetBucketLeuron leuron.resourceId leuron.id (not member))), P.checked member]]
              , H.div [P.class_ B.colXs2] [renderGravatarForUser Small (usersMapLookup_ToUser st leuron.userId)]
              , H.div [P.class_ B.colXs2] [linkToP [] (ResourcesLeurons leuron.resourceId (ShowI leuron.id) emptyParams) (show leuron.id)]
              , H.div [P.class_ B.colXs2] [H.p_ [H.text $ show $ leuronToTyLeuron leuron.dataP]]
              , H.div [P.classes [B.colXs3, B.hiddenXs]] [H.p_ [H.text $ show leuron.createdAt]]
              , H.div [P.classes [B.colXs2, B.hiddenXs]] []
            ],
            H.div [P.class_ B.row] [
              H.div [P.class_ B.colXs1] [],
              H.div [P.class_ B.colXs9] [renderLeuron $ pack ^. _LeuronPackResponse .. leuron_],
              H.div [P.class_ B.colXs1] []
            ]
          ])
        $ listToArray $ Map.values st.leurons
    , renderPageNumbers st.leuronsPageInfo st.currentPage
  ]
  where
  m = st.bucketLeurons
