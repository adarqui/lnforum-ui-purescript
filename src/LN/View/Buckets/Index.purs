module LN.View.Buckets.Index (
  renderView_Buckets_Index
) where



import LN.ArrayList           (listToArray)
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
import LN.State.Loading                (getLoading, l_buckets)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Size(Small)
                                       , _BucketPackResponse, _BucketResponse
                                       , bucket_)



renderView_Buckets_Index :: State -> ComponentHTML Input
renderView_Buckets_Index st =

  case getLoading l_buckets st.loading of
       true  -> renderLoading
       false -> renderView_Buckets_Index' st



renderView_Buckets_Index' :: State -> ComponentHTML Input
renderView_Buckets_Index' st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Buckets"]
    ],

    -- H.div [P.classes [B.colLg2, B.colMd2, B.colXs12]] [
    H.div [P.class_ B.row] [
      linkToP_Classes [B.btn, B.btnLg, B.btnInfo, B.btnBlock] [] (Buckets New emptyParams) "new"
    ],

-- TODO FIXME: Bring back renderOrderBy once we figure out how we want to sort (LN.Sort)
-- Page Numbers
--    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],

    -- Buckets
    H.div [] [buckets st]
  ]



buckets :: State -> ComponentHTML Input
buckets st =
  H.div [P.class_ B.containerFluid] [
    renderPageNumbers st.bucketsPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\pack ->
          let
            bucket_pack = pack ^. _BucketPackResponse
            bucket      = pack ^. _BucketPackResponse .. bucket_ ^. _BucketResponse
          in
          H.li_ [
            H.div [P.class_ B.row] [
                H.div [P.class_ B.colSm1] [
                  renderGravatarForUser Small (usersMapLookup_ToUser st bucket.userId)
                ]
              , H.div [P.classes [B.colSm9]] [
                    H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (Buckets (Show $ show bucket.id) emptyParams) bucket.displayName]
                  , H.p_ [H.text $ show bucket.createdAt]
                  , H.p_ [H.text $ show bucket.description]
                ]
              , H.div [P.classes [B.colSm2, B.hiddenXs]] [
                  H.p_ [H.text "empty_stats"]
                ]
            ]
          ])
        $ listToArray $ M.values st.buckets
    , renderPageNumbers st.bucketsPageInfo st.currentPage
  ]
