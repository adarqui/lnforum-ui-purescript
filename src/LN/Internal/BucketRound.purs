module LN.Internal.BucketRound (
  defaultBucketRoundRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(..))
import Prelude                     (class Eq, class Show, eq, show)

import LN.T                        (BucketRoundRequest, mkBucketRoundRequest)



defaultBucketRoundRequest :: BucketRoundRequest
defaultBucketRoundRequest = mkBucketRoundRequest [] 3 0 0
