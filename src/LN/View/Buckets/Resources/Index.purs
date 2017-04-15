module LN.View.Buckets.Resources.Index (
  renderView_Buckets_Resources_Index
) where



import LN.ArrayList           (listToArray)
import Data.Map                        as Map
import Data.Maybe
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.HTML.Events     as E
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, map, not, ($), (<>), (<<<))

import LN.Input.Types                  (Input, cBucketMod)
import LN.Input.Bucket
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Size(Small)
                                       , ResourcePackResponse(..), _ResourceStatResponse, _ResourcePackResponse, _ResourceResponse
                                       , stat_, resource_)



renderView_Buckets_Resources_Index :: Int -> State -> ComponentHTML Input
renderView_Buckets_Resources_Index bucket_id st =
  if Map.isEmpty st.resources
     then renderLoading
     else renderView_Buckets_Resources_Index' bucket_id st



renderView_Buckets_Resources_Index' :: Int -> State -> ComponentHTML Input
renderView_Buckets_Resources_Index' bucket_id st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Resources"]
    ],

    H.div [P.classes [B.colLg2, B.colMd2, B.colXs12]] [
      linkToP_Classes [B.btn, B.btnLg, B.btnInfo, B.btnBlock] [] (Resources New emptyParams) "new"
    ],

-- TODO FIXME: Bring back renderOrderBy once we figure out how we want to sort (LN.Sort)
-- Page Numbers
--    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],

    -- Resources
    H.div [] [resources bucket_id st]
  ]



resources :: Int -> State -> ComponentHTML Input
resources bucket_id st =

  H.div [P.class_ B.containerFluid] [
    renderPageNumbers st.resourcesPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\pack ->
          let
            resource_pack = pack ^. _ResourcePackResponse
            resource      = pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse
            stat          = pack ^. _ResourcePackResponse .. stat_ ^. _ResourceStatResponse
            member        = Map.member resource.id m
          in
          H.li_ [
            H.div [P.class_ B.row] [
              H.div [P.class_ B.colSm1] [H.input [P.type_ P.InputCheckbox, P.name "select-resource", P.value "",
                                         E.onChecked (E.input_ (cBucketMod $ SetBucketResource resource.id (not member))), P.checked member]]
              , H.div [P.class_ B.colSm1] [renderGravatarForUser Small (usersMapLookup_ToUser st resource.userId)]
              , H.div [P.classes [B.colSm8]] [
                    H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (Resources (Show $ show resource.id) emptyParams) resource.displayName]
                  , H.p_ [H.text $ show resource.createdAt]
                  , H.p_ [H.text $ resource.description]
                ]
              , H.div [P.classes [B.colSm2, B.hiddenXs]] [H.p_ [H.text $ show stat.leurons <> " leurons"]]
            ]
          ])
        $ listToArray $ Map.values st.resources
    , renderPageNumbers st.resourcesPageInfo st.currentPage
  ]
  where
  m = st.bucketResources
