module LN.View.Buckets.Rounds.Mod (
  renderView_Buckets_Rounds_New,
  renderView_Buckets_Rounds_Mod
) where



import Data.Array                      as Array
import Data.Array                      ((:))
import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, flip, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Internal.Bucket
import LN.Input.BucketRound            (InputBucketRound(..), BucketRound_Mod(..))
import LN.Input.Types                  (Input(..), cBucketRoundMod, cBucketRound)
import LN.State.Loading                (getLoading, l_currentBucketRound)
import LN.State.BucketRound            (BucketRoundRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Fields
import LN.View.Helpers
import LN.Router.Class.Routes
import LN.T



renderView_Buckets_Rounds_New :: Int -> State -> ComponentHTML Input
renderView_Buckets_Rounds_New bucket_id = renderView_Buckets_Rounds_Mod bucket_id



renderView_Buckets_Rounds_Mod :: Int ->State -> ComponentHTML Input
renderView_Buckets_Rounds_Mod bucket_id st =
  case st.currentBucketRoundRequest, st.currentBucketRoundRequestSt, getLoading l_currentBucketRound st.loading of
    _, _, true                               -> renderLoading
    Just bucket_round_req, Just rst, false   -> renderView_Buckets_Rounds_Mod' bucket_id bucket_round_req rst st
    _, _, false                              -> H.div_ [H.p_ [H.text "Buckets_Rounds_Mod: unexpected error."]]



renderView_Buckets_Rounds_Mod' :: Int -> BucketRoundRequest -> BucketRoundRequestState -> State -> ComponentHTML Input
renderView_Buckets_Rounds_Mod' bucket_id bucket_round_req rst st =
  H.div_ [

    H.h1_ [ H.text "Add Round" ]

  , multipleCheckboxMenu
      "Training Styles"
      "training-styles"
      [ "simple", "boolean", "matching", "subs", "splits" ]
      (\s ->
        let new_styles =
              case s of
                "simple"   -> Array.nub $ TS_Simple : styles
                "boolean"  -> Array.nub $ TS_Boolean : styles
                "matching" -> Array.nub $ TS_Match : styles
                "subs"     -> Array.nub $ TS_Subs : styles
                "splits"   -> Array.nub $ TS_Splits : styles
                _          -> styles
        in cBucketRoundMod $ ModReq (\(BucketRoundRequest req) -> BucketRoundRequest (req { trainingStyles = new_styles})))
      $ map (flip Array.elem styles) [TS_Simple, TS_Boolean, TS_Match, TS_Subs, TS_Splits ]

  , buttons_CreateCancel (Just bucket_id) (cBucketRoundMod $ Create) About

  ]
  where
  bucket_round = unwrapBucketRoundRequest bucket_round_req
  styles = bucket_round_req ^. _BucketRoundRequest .. trainingStyles_
