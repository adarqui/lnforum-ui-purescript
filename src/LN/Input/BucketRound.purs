module LN.Input.BucketRound (
  InputBucketRound (..),
  BucketRound_Mod (..)
) where



import Data.Maybe         (Maybe)

import LN.State.BucketRound    (BucketRoundRequestState)



data InputBucketRound
  = InputBucketRound_Nop1
  | InputBucketRound_Mod BucketRound_Mod
  | InputBucketRound_Nop



data BucketRound_Mod
  = ModSt (BucketRoundRequestState -> BucketRoundRequestState)

  | Create
  | EditP Int -- edit bucket_round_id
