module LN.State.Loading (
  LoadingMap,
  defaultLoadingMap,
  getLoading,
  setLoading,
  clearLoading,

  l_me,
  l_users,
  l_resources,
  l_leurons,
  l_buckets,
  l_bucketRounds,
  l_bucketNodes,
  l_leuronNodes,

  l_currentUser,

  l_currentResource,
  l_currentResourceRequest,

  l_currentLeuron,
  l_currentLeuronRequest,

  l_currentBucket,
  l_currentBucketRequest,

  l_currentBucketRound,
  l_currentBucketRoundRequest,

  l_currentBucketNode,
  l_currentBucketNodeRequest
) where



import LN.ArrayList          (arrayToList)
import Data.List             (List)
import Data.Maybe            (Maybe(..), fromJust)
import Data.Map              as M
import Data.Tuple            (Tuple(..))
import Prelude               (map, const, ($))



type LoadingMap = M.Map Int Boolean



defaultLoadingMap :: LoadingMap
defaultLoadingMap = M.fromFoldable $ map (\s -> Tuple s false) loadingKeys



getLoading :: Int -> LoadingMap -> Boolean
getLoading key lm = case M.lookup key lm of
                         Just v -> v
                         _      -> false



setLoading :: Int -> LoadingMap -> LoadingMap
setLoading key lm = M.update (const $ Just true) key lm



clearLoading :: Int -> LoadingMap -> LoadingMap
clearLoading key lm = M.update (const $ Just false) key lm




loadingKeys :: List Int
loadingKeys =
  arrayToList $
    [ l_me
    , l_users
    , l_resources
    , l_leurons
    , l_currentUser
    , l_currentResource
    , l_currentResourceRequest
    , l_currentLeuron
    , l_currentLeuronRequest
    ]



-- TODO FIXME: This should be an Enum

l_me                            :: Int
l_me                            = 0

l_users                         :: Int
l_users                         = 1

l_resources                     :: Int
l_resources                     = 2

l_leurons                       :: Int
l_leurons                       = 3

l_currentUser                   :: Int
l_currentUser                   = 4

l_currentResource               :: Int
l_currentResource               = 5

l_currentResourceRequest        :: Int
l_currentResourceRequest        = 6

l_currentLeuron                 :: Int
l_currentLeuron                 = 7

l_currentLeuronRequest          :: Int
l_currentLeuronRequest          = 8

l_buckets                       :: Int
l_buckets                       = 9

l_currentBucket                 :: Int
l_currentBucket                 = 10

l_currentBucketRequest          :: Int
l_currentBucketRequest          = 11

l_bucketRounds                  :: Int
l_bucketRounds                  = 12

l_currentBucketRound            :: Int
l_currentBucketRound            = 13

l_currentBucketRoundRequest     :: Int
l_currentBucketRoundRequest     = 14

l_bucketNodes                   :: Int
l_bucketNodes                   = 15

l_currentBucketNode             :: Int
l_currentBucketNode             = 16

l_currentBucketNodeRequest      :: Int
l_currentBucketNodeRequest      = 17

l_leuronNodes                   :: Int
l_leuronNodes                   = 15

l_currentLeuronNode             :: Int
l_currentLeuronNode             = 16

l_currentLeuronNodeRequest      :: Int
l_currentLeuronNodeRequest      = 17
