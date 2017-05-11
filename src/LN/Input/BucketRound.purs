module LN.Input.BucketRound (
  InputBucketRound (..),
  BucketRound_Mod (..)
) where



import Data.Maybe              (Maybe)

import LN.State.BucketRound    (BucketRoundRequestState)
import LN.T                    (BucketRoundRequest)



data InputBucketRound
  = InputBucketRound_Nop1
  | InputBucketRound_Mod BucketRound_Mod
  | InputBucketRound_GetLeuron
  | InputBucketRound_Nop



data BucketRound_Mod
  = ModSt (BucketRoundRequestState -> BucketRoundRequestState)
  | ModReq (BucketRoundRequest -> BucketRoundRequest)

  | Create
  | EditP Int -- edit bucket_round_id
