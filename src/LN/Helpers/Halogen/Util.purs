module LN.Helpers.Halogen.Util (
  dataToggle,
  dataHelper,
  ariaHelper,
  row,
  col,
  col',
  container,
  container_
) where



import Data.Array                      (range, concat, (:))
import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            (HTML(), ClassName())
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



row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]



col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]



col' :: forall a b. Array ClassName -> Array (HTML a b) -> HTML a b
col' szs = H.div [ P.classes szs ]



-- container :: Array IProp -> HTML _ _
container attrs = H.div (P.class_ B.container : attrs)



container_ :: Array (HTML _ _) -> HTML _ _
container_ = container []
