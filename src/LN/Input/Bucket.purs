module LN.Input.Bucket (
  InputBucket (..),
  Bucket_Mod (..)
) where



import Data.Maybe         (Maybe)

import LN.State.Bucket    (BucketRequestState)



data InputBucket
  = InputBucket_Nop1
  | InputBucket_Mod Bucket_Mod
  | InputBucket_Nop



data Bucket_Mod
  = SetDisplayName String
  | EditDisplayName String
  | RemoveDisplayName

  | SetDescription String
  | EditDescription String
  | RemoveDescription

  | Save
  | EditP Int -- edit bucket_id
