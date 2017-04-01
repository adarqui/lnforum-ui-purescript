module LN.Internal.Bucket (
  defaultBucketRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(..))
import Prelude                     (class Eq, class Show, eq, show)

import LN.T                        (BucketRequest, mkBucketRequest)



defaultBucketRequest :: BucketRequest
defaultBucketRequest = mkBucketRequest "" Nothing 0 0 [] [] [] [] 0
