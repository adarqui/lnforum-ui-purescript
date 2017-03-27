module LN.Component.Types (
--  ComponentSlot,
  UICompEff,
  CompEff,
  EvalEff,
--  EvalEffP,
  LNEff,
  LN
) where



import Control.Monad.Aff         (Aff())
import Control.Monad.Eff.Console (CONSOLE())
-- import Data.Date                 (Now())
-- import Data.Date.Locale          (Locale())
-- import Data.NaturalTransformation (Natural(..))
import Data.NaturalTransformation
import Halogen                   (Component, ComponentDSL)
import Halogen.Aff.Effects       (HalogenEffects)
import Halogen.HTML as HH
import Network.HTTP.Affjax       (AJAX())
import Prelude                   (Unit, Void)
import WebSocket                 (WEBSOCKET())
-- import Browser.WebStorage        (WebStorage())

import LN.Input.Types            (Input)
import LN.State.Types            (State)



-- type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s }


{-
18:08 < garyb> that's fine, it's `ComponentDSL` that has changed
18:08 < garyb> ComponentDSL <state> <query algebra> <output message> <monad> <value>
18:09 < garyb> likewise the actual Component type has changed:
18:09 < garyb> Component HH.HTML <query algebra> <input value> <output message> <monad>
-}

-- type EvalEff = forall eff. {-Partial =>-} Natural Input State Input (LNEff eff) -> Natural Input State Input (LNEff eff)
-- type CompEff = forall eff. Natural Input (ComponentDSL State Input (LNEff eff))
-- type CompEff = forall eff. Natural Input (ComponentDSL State Input)
-- type CompEff = forall eff. Natural Input (ComponentDSL State Input Void (LNEff eff))
-- type CompEff = forall eff. NaturalTransformation Input (ComponentDSL State Input Void (LNEff eff))
type UICompEff = forall eff. Component HH.HTML Input Unit Void (LNEff eff)
type CompEff = forall eff. Input ~> (ComponentDSL State Input Void (LNEff eff))
type EvalEff = CompEff -> CompEff
-- type EvalEff = forall eff. Natural Input (ComponentDSL State Input (LNEff eff)) -> Natural Input (ComponentDSL State Input (LNEff eff))
-- Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
--type EvalEffP = forall eff. Natural Input State Input (LNEff eff)

-- myButton :: forall m. H.Component HH.HTML Query Unit Message m
-- eval :: Query ~> H.ComponentDSL State Query Message m




type LNEff eff = Aff (LN eff)
type LN eff =
  HalogenEffects
  ( {-webStorage :: WebStorage -}
    ajax      :: AJAX
--    , now       :: Now
--    , locale    :: Locale
    , ws        :: WEBSOCKET
    , console   :: CONSOLE | eff
    )
