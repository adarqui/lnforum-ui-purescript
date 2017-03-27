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
  l_currentUser,
  l_currentResource,
  l_currentResourceRequest,
  l_currentLeuron,
  l_currentLeuronRequest
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
