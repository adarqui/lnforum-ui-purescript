module LN.State.Bucket (
  BucketRequestState,
  defaultBucketRequestState
) where



import LN.Internal.Bucket
import LN.T



type BucketRequestState = {
  -- Show me my stuff only
  myStuff :: Boolean
}



defaultBucketRequestState :: BucketRequestState
defaultBucketRequestState = {
  myStuff: false
}
