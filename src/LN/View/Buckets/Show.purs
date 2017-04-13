module LN.View.Buckets.Show (
  renderView_Buckets_Show
) where



import Prelude                         (($), show)
import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentBucket)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( BucketPackResponse, _BucketPackResponse, _BucketResponse
                                       , bucket_)



renderView_Buckets_Show :: Int -> State -> ComponentHTML Input
renderView_Buckets_Show bucket_id st =

  case st.currentBucket, getLoading l_currentBucket st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "bucket unavailable."]]
       Just pack, false -> renderView_Buckets_Show' pack st



renderView_Buckets_Show' :: BucketPackResponse -> State -> ComponentHTML Input
renderView_Buckets_Show' pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [H.text (bucket.displayName)],
      H.p [P.class_ B.textCenter] [H.text $ show bucket.description]
    ],
    H.div [P.class_ B.container] [
      linkToP [] (Buckets (EditI bucket.id) emptyParams) "edit",
      linkToP [] (Buckets (DeleteI bucket.id) emptyParams) "delete"
    ],
    H.div [P.class_ B.container] [
      H.div [P.class_ B.listGroup] [
        linkToP [] (BucketsResources bucket.id Index emptyParams) "resources",
        linkToP [] (BucketsLeurons bucket.id Index emptyParams) "leurons",
        linkToP [] (BucketsTraining bucket.id Index emptyParams) "training"
      ]
    ]
  ]

 where
 bucket = pack ^. _BucketPackResponse .. bucket_ ^. _BucketResponse
