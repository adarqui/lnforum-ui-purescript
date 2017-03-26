module LN.Component.Util (
  quietLaunchAff
) where



import Control.Monad.Aff     (Aff(), runAff)
import Control.Monad.Eff     (Eff())
import Prelude               (Unit, const, pure, bind, unit)



quietLaunchAff :: forall eff a. Aff eff a -> Eff eff Unit
quietLaunchAff aff = do
  runAff (const (pure unit)) (const (pure unit)) aff
  pure unit
