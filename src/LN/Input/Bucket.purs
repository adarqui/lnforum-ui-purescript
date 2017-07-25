module LN.Input.Bucket (
  InputBucket (..),
  Bucket_Mod (..)
) where




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

  | SetBucketResource Int Boolean
  | SetBucketLeuron Int Int Boolean

  | ModSt (BucketRequestState -> BucketRequestState)

  | Create
  | EditP Int -- edit bucket_id
