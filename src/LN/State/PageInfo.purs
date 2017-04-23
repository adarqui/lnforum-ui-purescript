module LN.State.PageInfo (
  PageInfo,
  defaultPageInfo,
  defaultPageInfo_Users,
  defaultPageInfo_Resources,
  defaultPageInfo_Leurons,
  defaultPageInfo_Buckets,
  defaultPageInfo_BucketRounds,
  RunPageInfo,
  runPageInfo
) where



import Data.Array          (head)
import Data.Maybe          (maybe)
import Optic.Core          ((^.), (..))
import Prelude             ((+), (-), (/), (*))
import LN.T
import LN.T.Param

{-
import LN.T.Internal.Types (SortOrderBy(..), OrderBy(..), CountResponses, Param(..)
                           , _CountResponses, _CountResponse, countResponses_, n_)
                           -}



type PageInfo =
  { currentPage :: Int
  , resultsPerPage :: Int
  , totalResults :: Int
  , totalPages :: Int
  , sortOrder :: SortOrderBy
  , order :: OrderBy
  }



defaultPageInfo :: PageInfo
defaultPageInfo = { currentPage: 1, resultsPerPage: 20, totalResults: 0, totalPages: 1, sortOrder: SortOrderBy_Asc, order: OrderBy_Id }



defaultPageInfo_Users :: PageInfo
defaultPageInfo_Users = defaultPageInfo



defaultPageInfo_Resources :: PageInfo
defaultPageInfo_Resources = defaultPageInfo



defaultPageInfo_Leurons :: PageInfo
defaultPageInfo_Leurons = defaultPageInfo



defaultPageInfo_Buckets :: PageInfo
defaultPageInfo_Buckets = defaultPageInfo



defaultPageInfo_BucketRounds :: PageInfo
defaultPageInfo_BucketRounds = defaultPageInfo { sortOrder = SortOrderBy_Dsc, order = OrderBy_CreatedAt }



-- For page numbers stuff, inside Eval components

type RunPageInfo = {
  count    :: Int,
  pageInfo :: PageInfo,
  params   :: Array Param
}



runPageInfo :: CountResponses -> PageInfo -> RunPageInfo
runPageInfo count_responses page_info =
  {
    count: count,
    pageInfo: pi,
    params: par
  }
  where
  count =
    maybe
      0
      (\count_response -> count_response ^. _CountResponse .. n_)
      (head (count_responses ^. _CountResponses .. countResponses_))
  pi  =
    page_info {
      totalResults = count,
      totalPages   = (count / page_info.resultsPerPage) + 1
    }
  par =
    [ Limit page_info.resultsPerPage
    , Offset ((page_info.currentPage - 1) * page_info.resultsPerPage)
    , SortOrder page_info.sortOrder
    , Order page_info.order
    ]
