module LN.Helpers.Halogen.Util (
  dataToggle,
  dataHelper,
  ariaHelper
) where



import Data.Array                      (range, concat)
import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Core               as C
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (show, map, ($), (<>))
import Unsafe.Coerce                   as U



dataToggle :: forall i r. String -> P.IProp r i
dataToggle = U.unsafeCoerce dtoggle
  where
  dtoggle = C.attr (C.AttrName "data-toggle")



dataHelper :: forall i r. String -> String -> P.IProp r i
dataHelper prefix = U.unsafeCoerce dhelper
  where
  dhelper = C.attr (C.AttrName $ "data-" <> prefix)



ariaHelper :: forall i r. String -> String -> P.IProp r i
ariaHelper prefix = U.unsafeCoerce dhelper
  where
  dhelper = C.attr (C.AttrName $ "aria-" <> prefix)
