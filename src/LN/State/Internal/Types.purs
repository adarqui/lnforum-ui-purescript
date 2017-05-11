module LN.State.Internal.Types (
  InternalState
) where



import Data.Map                     as M
import Data.Maybe                   (Maybe)
import Data.Tuple                   (Tuple)
import Prelude                      (Unit)

import LN.State.ArrayString         (ArrayStringState)
import LN.State.Bucket              (BucketRequestState)
import LN.State.BucketRound         (BucketRoundRequestState)
import LN.State.Leuron              (LeuronRequestState)
import LN.State.Loading             (LoadingMap)
import LN.State.Resource            (ResourceRequestState)
import LN.State.PageInfo            (PageInfo)
import LN.T



type InternalState routes {- TODO FIXME: driver_ch-} =
  { currentPage                  :: routes
  , me                           :: Maybe UserPackResponse
  , meId                         :: Int
  , errors                       :: Array (Tuple String String)
  , users                        :: M.Map Int UserSanitizedPackResponse
  , usersMap                     :: M.Map Int UserSanitizedPackResponse
  , resources                    :: M.Map Int ResourcePackResponse
  , leurons                      :: M.Map Int LeuronPackResponse
  , buckets                      :: M.Map Int BucketPackResponse
  , bucketResources              :: M.Map Int Unit
  , bucketLeurons                :: M.Map Int Unit
  , bucketRounds                 :: M.Map Int BucketRoundResponse
  , currentUser                  :: Maybe UserSanitizedPackResponse
  , currentResource              :: Maybe ResourcePackResponse
  , currentResourceRequest       :: Maybe ResourceRequest
  , currentResourceRequestSt     :: Maybe ResourceRequestState
  , currentLeuron                :: Maybe LeuronPackResponse
  , currentLeuronRequest         :: Maybe LeuronRequest
  , currentLeuronRequestSt       :: Maybe LeuronRequestState
  , currentBucket                :: Maybe BucketPackResponse
  , currentBucketRequest         :: Maybe BucketRequest
  , currentBucketRequestSt       :: Maybe BucketRequestState
  , currentBucketRound           :: Maybe BucketRoundResponse
  , currentBucketRoundRequest    :: Maybe BucketRoundRequest
  , currentBucketRoundRequestSt  :: Maybe BucketRoundRequestState
  , currentBucketRoundLeuronsCount :: Int
  , currentPageInfo              :: PageInfo
  , usersPageInfo                :: PageInfo
  , resourcesPageInfo            :: PageInfo
  , leuronsPageInfo              :: PageInfo
  , bucketsPageInfo              :: PageInfo
  , bucketRoundsPageInfo         :: PageInfo
  , arrayStringSt                :: ArrayStringState
  , loading                      :: LoadingMap
  }
