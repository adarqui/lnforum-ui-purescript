module LN.Helpers.Api (
  rd
) where



import Data.Either                     (Either(..))
import Data.Maybe                      (Maybe(..))
import Halogen                         (modify)
import Halogen as H
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, ($))

import LN.Api                          (getMePack')
import LN.Api.Helpers                  (rd)
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T                            (_UserPackResponse, userId_)


import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Aff          (Aff())
import Control.Monad.Aff.Free     (class Affable, fromAff)
import Prelude                    ((<<<))
import Purescript.Api.Helpers     (ApiOptions, rD)


-- rd_ :: forall a b c. ReaderT ApiOptions (Aff a) c -> b c
-- rd_ = rD

rd = H.liftAff <<< rD
