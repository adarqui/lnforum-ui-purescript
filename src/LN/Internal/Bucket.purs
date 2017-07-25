module LN.Internal.Bucket (
  defaultBucketRequest
) where



import Data.Maybe                  (Maybe(..))

import LN.T                        (BucketRequest, mkBucketRequest)



defaultBucketRequest :: BucketRequest
defaultBucketRequest = mkBucketRequest "" Nothing 0 0 [] [] [] [] 0
