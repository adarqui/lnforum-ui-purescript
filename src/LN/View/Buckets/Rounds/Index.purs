module LN.View.Buckets.Rounds.Index (
  renderView_Buckets_Rounds_Index
) where



import LN.ArrayList                    (listToArray)
import Data.Map                        as M
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, map, ($), (<>))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_bucketRounds)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Size(Small)
                                       , BucketRoundResponse(..), _BucketRoundResponse
                                       , bucketRound_)



renderView_Buckets_Rounds_Index :: Int -> State -> ComponentHTML Input
renderView_Buckets_Rounds_Index bucket_id st =

  case getLoading l_bucketRounds st.loading of
       true  -> renderLoading
       false -> renderView_Buckets_Rounds_Index' bucket_id st



renderView_Buckets_Rounds_Index' :: Int -> State -> ComponentHTML Input
renderView_Buckets_Rounds_Index' bucket_id st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Buckets"]
    ],

    -- H.div [P.classes [B.colLg2, B.colMd2, B.colXs12]] [
    H.div [P.class_ B.row] [
      linkToP_Classes [B.btn, B.btnLg, B.btnInfo, B.btnBlock] [] (Buckets New emptyParams) "new"
    ],

    -- Rounds
    H.div [] [rounds bucket_id st]
  ]



rounds :: Int -> State -> ComponentHTML Input
rounds bucket_id st =
  H.div [P.class_ B.containerFluid] [
    renderPageNumbers st.bucketsPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\(BucketRoundResponse round) ->
          H.li_ [
            H.div [P.class_ B.row] [
              H.div [P.classes [B.colSm9]] [
                    H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (BucketsRounds bucket_id (Show $ show round.id) emptyParams) $ show round.id]
                  , H.p_ [H.text $ show round.createdAt]
                ]
              , H.div [P.classes [B.colSm2, B.hiddenXs]] [
                  H.p_ [H.text "empty_stats"]
                ]
            ]
          ])
        $ listToArray $ M.values st.bucketRounds
    , renderPageNumbers st.bucketRoundsPageInfo st.currentPage
  ]
