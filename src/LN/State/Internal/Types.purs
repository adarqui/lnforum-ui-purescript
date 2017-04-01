module LN.State.Internal.Types (
  InternalState
) where



-- import Control.Monad.Aff.AVar       (AVar())
import Data.Map                     as M
import Data.Maybe                   (Maybe)
import Data.Tuple                   (Tuple)

-- import LN.Input.Types               (Input)
-- import LN.Router.Types              (Routes(..))
import LN.State.ArrayString         (ArrayStringState)
import LN.State.Bucket              (BucketRequestState)
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
--  , workouts                   :: M.Map Int WorkoutPackResponse
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
  , currentPageInfo              :: PageInfo
  , usersPageInfo                :: PageInfo
  , resourcesPageInfo            :: PageInfo
  , leuronsPageInfo              :: PageInfo
  , bucketsPageInfo              :: PageInfo
  , arrayStringSt                :: ArrayStringState
--  , driverCh                     :: driver_ch
--  , driverCh                   :: AVar (Input Unit)
  , loading                      :: LoadingMap
  }
