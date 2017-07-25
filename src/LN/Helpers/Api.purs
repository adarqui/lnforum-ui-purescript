module LN.Helpers.Api (
  rd
) where



import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Reader.Trans (ReaderT)
import Halogen as H
import Prelude                    ((<<<))

import Purescript.Api.Helpers (ApiOptions, rD)


-- rd_ :: forall a b c. ReaderT ApiOptions (Aff a) c -> b c
-- rd_ = rD

rd :: forall t5 t6 t7. MonadAff t5 t6 => ReaderT ApiOptions (Aff t5) t7 -> t6 t7
rd = H.liftAff <<< rD
