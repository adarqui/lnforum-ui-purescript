module LN.Eval.OrderBy (
  eval_OrderBy
) where



import Prelude                   (pure)

import LN.Component.Types        (EvalEff)
import LN.Input.OrderBy          (InputOrderBy(..))
import LN.Input.Types            (Input(..))



eval_OrderBy :: Partial => EvalEff



eval_OrderBy eval (CompOrderBy (InputOrderBy_Set order_by) next) = do
  pure next
