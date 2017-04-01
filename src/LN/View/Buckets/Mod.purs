module LN.View.Buckets.Mod (
  renderView_Buckets_Delete,
  renderView_Buckets_New,
  renderView_Buckets_Edit,
  renderView_Buckets_Mod
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Internal.Bucket
import LN.Input.Bucket               (InputBucket(..), Bucket_Mod(..))
import LN.Input.Types                  (Input(..), cBucketMod)
import LN.State.Loading                (getLoading, l_currentBucket)
import LN.State.Bucket               (BucketRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Fields
import LN.View.Helpers
import LN.Router.Class.Routes
import LN.T



renderView_Buckets_Delete :: Int -> State -> ComponentHTML Input
renderView_Buckets_Delete bucket_id st =

  case st.currentBucket, getLoading l_currentBucket st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "bucket unavailable."]]
       Just pack, false -> renderView_Buckets_Delete' pack st



renderView_Buckets_Delete' :: BucketPackResponse -> State -> ComponentHTML Input
renderView_Buckets_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 bucket = pack ^. _BucketPackResponse .. bucket_ ^. _BucketResponse



renderView_Buckets_New :: State -> ComponentHTML Input
renderView_Buckets_New = renderView_Buckets_Mod Nothing



renderView_Buckets_Edit :: Int -> State -> ComponentHTML Input
renderView_Buckets_Edit bucket_id = renderView_Buckets_Mod (Just bucket_id)



renderView_Buckets_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_Buckets_Mod m_bucket_id st =
  case st.currentBucketRequest, st.currentBucketRequestSt, getLoading l_currentBucket st.loading of
    _, _, true                         -> renderLoading
    Just bucket_req, Just rst, false -> renderView_Buckets_Mod' m_bucket_id bucket_req rst st
    _, _, false                        -> H.div_ [H.p_ [H.text "Buckets_Mod: unexpected error."]]



renderView_Buckets_Mod' :: Maybe Int -> BucketRequest -> BucketRequestState -> State -> ComponentHTML Input
renderView_Buckets_Mod' m_bucket_id bucket_req rst st =
  H.div_ [

    H.h1_ [ H.text "Add Bucket" ]

  , input_Label "Name" "Name" bucket.displayName P.InputText (E.input (cBucketMod <<< SetDisplayName))

  , optionalDescriptionField bucket.description (cBucketMod <<< SetDescription) (cBucketMod RemoveDescription)

  , buttons_CreateEditCancel m_bucket_id (cBucketMod $ Create) (cBucketMod <<< EditP) About
--  , simpleInfoButton save (cBucketMod $ Save m_bucket_id)

  ]
  where
  bucket = unwrapBucketRequest bucket_req
  save     = maybe "Create" (const "Save") m_bucket_id
