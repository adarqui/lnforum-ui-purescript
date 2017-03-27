module LN.Ent (
  createByParamFromEnt,
  createResyncFromEnt
) where



import Data.Maybe          (Maybe(..))
import Prelude             (($))

import LN.Input.Types
import LN.T                (Param(..), Ent(..))



createByParamFromEnt :: Ent -> Int -> Maybe Param
createByParamFromEnt ent ent_id =
  case ent of
       _              -> Nothing



createResyncFromEnt :: forall a. Ent -> Int -> a -> Input a
createResyncFromEnt ent ent_id next =
  case ent of
       -- TODO FIXME
       _ -> Nop next
--       _ -> pure next
       -- Ent_ThreadPost -> (cThreadPostAct (ThreadPost.ResyncById ent_id) next)
