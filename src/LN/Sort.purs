module LN.Sort (
  sortMapBy
) where



import Data.Array   as A
import Data.Map     as M
import Optic.Core   ((^.), (..))
import Prelude      (compare, ($), (<<<))

import LN.ArrayList (listToArray)
import LN.T



sortMapBy cmp posts_map =
  A.sortBy (\t1 t2 -> compare (cmp t1) (cmp t2)) $ listToArray $ M.values posts_map
