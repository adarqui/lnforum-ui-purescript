module LN.State.Bucket (
  BucketRequestState,
  defaultBucketRequestState
) where






type BucketRequestState = {
  -- Show me my stuff only
  myStuff :: Boolean
}



defaultBucketRequestState :: BucketRequestState
defaultBucketRequestState = {
  myStuff: false
}
