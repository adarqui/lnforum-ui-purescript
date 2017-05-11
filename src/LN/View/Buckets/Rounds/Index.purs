module LN.View.Buckets.Rounds.Index (
  renderView_Buckets_Rounds_Index
) where



import Data.Array                      as Array
import Data.Map                        as M
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, map, ($), (<>))

import LN.ArrayList                    (listToArray)
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
                                       , bucketRound_
                                       , _TrainingNode)



renderView_Buckets_Rounds_Index :: Int -> State -> ComponentHTML Input
renderView_Buckets_Rounds_Index bucket_id st =

  case getLoading l_bucketRounds st.loading of
       true  -> renderLoading
       false -> renderView_Buckets_Rounds_Index' bucket_id st



renderView_Buckets_Rounds_Index' :: Int -> State -> ComponentHTML Input
renderView_Buckets_Rounds_Index' bucket_id st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Rounds"]
    ],

    -- H.div [P.classes [B.colLg2, B.colMd2, B.colXs12]] [
    H.div [P.class_ B.row] [
      linkToP_Classes [B.btn, B.btnLg, B.btnInfo, B.btnBlock] [] (BucketsRounds bucket_id New emptyParams) "Go!"
    ],

    -- Rounds
    H.div [] [rounds bucket_id st]
  ]



rounds :: Int -> State -> ComponentHTML Input
rounds bucket_id st =
  H.div [P.class_ B.containerFluid] [
    renderPageNumbers st.bucketRoundsPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\(BucketRoundResponse round) ->
          let
            tr = round.trainingNode ^. _TrainingNode
          in
            H.li_ [
              H.div [P.class_ B.row] [
                H.div [P.classes [B.colSm12]] [
                      H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (BucketsRounds bucket_id (Show $ show round.id) emptyParams) $ show round.id]
                    , H.p_ [H.text $ "styles: " <> show round.trainingStyles]
                    , H.p_ [H.text $ "threshold: " <> show round.threshold]
                    , H.p_ [H.text $ "numTotal: " <> show tr.numTotal]
                    , H.p_ [H.text $ "numKnow: " <> show tr.numKnow]
                    , H.p_ [H.text $ "numDontKnow: " <> show tr.numDontKnow]
                    , H.p_ [H.text $ "numDontCare: " <> show tr.numDontCare]
                    , H.p_ [H.text $ "numProtest: " <> show tr.numProtest]
                    , H.p_ [H.text $ "createdAt: " <> show round.createdAt]
                ]
              ]
            ])
        -- reverse: because we order by id, descending .. and we're converting a map to a list
        $ Array.reverse $ listToArray $ M.values st.bucketRounds
    , renderPageNumbers st.bucketRoundsPageInfo st.currentPage
  ]
