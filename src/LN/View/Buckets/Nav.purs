module LN.View.Buckets.Nav (
  renderView_Buckets_Nav
) where



import Prelude                         (($), show, id, not)
import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))

import LN.Input.Types                  (Input, cBucketMod)
import LN.Input.Bucket
import LN.Router.Link                  (linkToP_Classes, linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentBucket)
import LN.State.Types                  (State)
import LN.State.Bucket                 (BucketRequestState)
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( BucketPackResponse, _BucketPackResponse, _BucketResponse
                                       , bucket_)



renderView_Buckets_Nav :: BucketPackResponse -> State -> ComponentHTML Input
renderView_Buckets_Nav pack st =
  H.div [P.class_ B.container] [
    H.div [P.class_ B.listGroup] [
      linkToP [] (BucketsResources bucket.id Index emptyParams) "resources",
      linkToP [] (BucketsLeurons bucket.id Index emptyParams) "leurons",
      linkToP [] (BucketsRounds bucket.id Index emptyParams) "rounds",
      H.input [P.type_ P.InputCheckbox, P.name "my-stuff", P.value "",
              E.onChecked (E.input_ (cBucketMod $ ModSt modify_bst)), P.checked myStuff_checked]

    ]
  ]
 where
 bucket = pack ^. _BucketPackResponse .. bucket_ ^. _BucketResponse
 modify_bst vst = { myStuff: not vst.myStuff }
 myStuff_checked = case st.currentBucketRequestSt of
                        Nothing -> false
                        Just v -> v.myStuff
