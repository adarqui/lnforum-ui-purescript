module LN.Eval.Errors (
  eval_AddError,
  eval_AddErrorF,
  eval_AddErrorApi,
  eval_DelError,
  eval_ClearErrors
) where



import Data.Array                      (deleteAt)
import Data.Functor                    (($>))
import Data.Maybe                      (Maybe(..))
import Data.Tuple                      (Tuple(..))
import Halogen                         (gets, modify)
import Prelude                         (bind, pure, show, (<>))

import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))



eval_AddError :: Partial => EvalEff
eval_AddError eval (AddError author err next) = do

  modify (\st -> st{ errors = st.errors <> [Tuple author err] })
  pure next



-- | ForeignError helper
--
eval_AddErrorF :: Partial => EvalEff
eval_AddErrorF eval (AddErrorF author foreign_err next) = do

  eval (AddError author (show foreign_err) next)



-- | ApiError helper
--
eval_AddErrorApi :: Partial => EvalEff
eval_AddErrorApi eval (AddErrorApi author api_err next) = do

  eval (AddError author (show api_err) next)



eval_DelError :: Partial => EvalEff
eval_DelError eval (DelError index next) = do

  errors <- gets _.errors

  case deleteAt index errors of
       Nothing -> pure next
       Just arr -> modify (_{ errors = arr }) $> next



eval_ClearErrors :: Partial => EvalEff
eval_ClearErrors eval (ClearErrors next) = do

  modify (_{ errors = [] }) $> next
