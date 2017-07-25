module LN.Internal.BucketRound (
  defaultBucketRoundRequest
) where




import LN.T                        (BucketRoundRequest, mkBucketRoundRequest)



defaultBucketRoundRequest :: BucketRoundRequest
defaultBucketRoundRequest = mkBucketRoundRequest [] 3 0 0
