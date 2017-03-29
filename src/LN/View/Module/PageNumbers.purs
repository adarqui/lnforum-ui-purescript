module LN.View.Module.PageNumbers (
  renderPageNumbers,
  pageRange
) where



import Data.Array                      (range)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (show, map, ($), (+), (-), (<), (>), (<=), (>=), (==), (/=), (<>), (&&), (||), (*))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes')
import LN.Router.Types                 (Routes)
import LN.State.PageInfo               (PageInfo)
import LN.T



pageRange :: PageInfo -> Array Int
pageRange pageInfo =
  if total_pages > (ranger*2)
     then first_page <> range min_ranger max_ranger <> last_page
     else range 1 pageInfo.totalPages
  where
  current_page = pageInfo.currentPage
  total_pages  = pageInfo.totalPages
  ranger       = 7
  first_page   = [1]
  last_page    = if current_page == total_pages then [] else [total_pages]
  min_ranger   = if current_page - ranger <= 1 then 2 else current_page - ranger
  max_ranger   = if current_page + ranger > total_pages then total_pages
                                                        else if (current_page + ranger) < (ranger*2) then (ranger*2) else current_page + ranger



renderPageNumbers :: PageInfo -> Routes -> ComponentHTML Input
renderPageNumbers pageInfo route =
  H.div [P.class_ B.containerFluid] [
    H.ul [P.classes [B.pagination, B.paginationSm]]
    $
      map (\a ->
        a
      ) $ renderPageNumbers' pageInfo route
  ]



renderPageNumbers' :: PageInfo -> Routes -> Array (HTML _ _)
renderPageNumbers' pageInfo route =

  [H.li [] [linkToP_Classes' [Limit pageInfo.resultsPerPage, Offset prev] route "prev"]]
  <>
  (map (\page ->
      H.li (classes page) [linkToP_Classes' [Limit pageInfo.resultsPerPage, Offset page] route (show page)]
  ) $ pageRange pageInfo)
  <>
  [H.li [] [linkToP_Classes' [Limit pageInfo.resultsPerPage, Offset next] route "next"]]
  where
  prev = let p = (pageInfo.currentPage - 1) in if p < 1 then 1 else p
  next = let p = (pageInfo.currentPage + 1) in if (p > pageInfo.totalPages) then pageInfo.totalPages else p
  classes p =
    if p == pageInfo.currentPage
      then [P.classes $ [B.active] <> extra]
      else [P.classes extra]
    where
    extra = if (p >= pageInfo.currentPage - 2 && p <= pageInfo.currentPage + 2) || p == 1 || p == pageInfo.totalPages
               then []
               else [B.hiddenXs]
