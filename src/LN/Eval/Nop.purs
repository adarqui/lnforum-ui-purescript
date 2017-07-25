module LN.Eval.Nop (
  eval_Nop
) where



import Prelude (pure)

import LN.Component.Types        (EvalEff)
import LN.Input.Types            (Input(..))



eval_Nop :: Partial => EvalEff
eval_Nop eval (Nop next) = pure next
--  fromAff $ log "nop"
--  pure next
