module LN.View.Module.OrderBy (
  renderOrderBy
) where



import Data.Array                      (range, concat)
import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Core               as C
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (show, map, ($), (<>))
import Unsafe.Coerce                   as U

import LN.Input.Types                  (Input)
import LN.Helpers.Halogen.Util
import LN.Router.Link                  (linkToP_Classes')
import LN.Router.Types                 (Routes, orderBy)
import LN.T



renderOrderBy :: Routes -> ComponentHTML Input
renderOrderBy route =
  H.div [P.class_ B.dropdown] [
    H.button [dataToggle "dropdown", P.classes [B.btn, B.btnDefault, B.dropdownToggle]] [
      H.text "order", H.span [P.class_ B.caret] []
    ],
    H.ul [P.class_ B.dropdownMenu] $ concat $
      map (\order ->
        [
          H.li_ [
            linkToP_Classes' [SortOrder SortOrderBy_Asc, Order order] route $ (show order) <> " asc"
          ],
          H.li_ [
            linkToP_Classes' [SortOrder SortOrderBy_Dsc, Order order] route $ (show order) <> " dsc"
          ]
        ]
      ) (orderBy route)
  ]
