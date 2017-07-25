module LN.Sort (
  sortMapBy
) where



import Data.Array   as A
import Data.Map     as M
import Prelude (compare, ($))

import LN.ArrayList (listToArray)



sortMapBy :: forall t12 t5 t6. Ord t6 => (t5 -> t6) -> Map t12 t5 -> Array t5
sortMapBy cmp posts_map =
  A.sortBy (\t1 t2 -> compare (cmp t1) (cmp t2)) $ listToArray $ M.values posts_map
