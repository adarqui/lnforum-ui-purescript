module LN.Eval.Nop (
  eval_Nop,
  eval_Nop',
  eval_Nop''
) where



import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Free (fromAff)
import Data.NaturalTransformation
import Halogen
import Prelude                   (Void, bind, pure, ($), unit)

import LN.Component.Types        (EvalEff)
import LN.Input.Types            (Input(..))
import LN.State.Types


eval_Nop :: Partial => EvalEff
eval_Nop eval (Nop next) = pure next
--  fromAff $ log "nop"
--  pure next



eval_Nop' :: forall a. Partial => Input ~> ComponentDSL State Input Void a
eval_Nop' (Nop next) = pure next

{-- eval_Nop'' :: forall a. a ~> ComponentDSL State Input Void a --}
{-- eval_Nop'' next = pure next --}


-- eval_Nop' :: forall a. Partial => Input ~> ComponentDSL State Input Void a
eval_Nop'' = pure unit
