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



import Data.Array ((:))
import Data.Maybe                      (Maybe(..))
import Halogen.HTML            (HTML(), ClassName())
import Halogen.HTML.Core               as C
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Prelude (($), (<>))
import Unsafe.Coerce                   as U



dataToggle :: forall i r. String -> P.IProp r i
dataToggle = U.unsafeCoerce dtoggle
  where
  dtoggle = C.attr Nothing (C.AttrName "data-toggle")



dataHelper :: forall i r. String -> String -> P.IProp r i
dataHelper prefix = U.unsafeCoerce dhelper
  where
  dhelper = C.attr Nothing (C.AttrName $ "data-" <> prefix)



ariaHelper :: forall i r. String -> String -> P.IProp r i
ariaHelper prefix = U.unsafeCoerce dhelper
  where
  dhelper = C.attr Nothing (C.AttrName $ "aria-" <> prefix)



row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]



col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]



col' :: forall a b. Array ClassName -> Array (HTML a b) -> HTML a b
col' szs = H.div [ P.classes szs ]



-- container :: Array IProp -> HTML _ _
container :: forall t19 t20.
  Array
    (IProp
       ( "class" :: String
       , accessKey :: String
       , contentEditable :: Boolean
       , dir :: DirValue
       , draggable :: Boolean
       , gotPointerCapture :: Event
       , hidden :: Boolean
       , id :: String
       , lang :: String
       , lostPointerCapture :: Event
       , onBlur :: FocusEvent
       , onClick :: MouseEvent
       , onContextMenu :: Event
       , onCopy :: ClipboardEvent
       , onCut :: ClipboardEvent
       , onDoubleClick :: MouseEvent
       , onDrag :: DragEvent
       , onDragEnd :: DragEvent
       , onDragEnter :: DragEvent
       , onDragExit :: DragEvent
       , onDragLeave :: DragEvent
       , onDragOver :: DragEvent
       , onDragStart :: DragEvent
       , onDrop :: DragEvent
       , onFocus :: FocusEvent
       , onFocusIn :: FocusEvent
       , onFocusOut :: FocusEvent
       , onKeyDown :: KeyboardEvent
       , onKeyPress :: KeyboardEvent
       , onKeyUp :: KeyboardEvent
       , onMouseDown :: MouseEvent
       , onMouseEnter :: MouseEvent
       , onMouseLeave :: MouseEvent
       , onMouseMove :: MouseEvent
       , onMouseOut :: MouseEvent
       , onMouseOver :: MouseEvent
       , onMouseUp :: MouseEvent
       , onPaste :: ClipboardEvent
       , onPointerCancel :: Event
       , onPointerDown :: Event
       , onPointerEnter :: Event
       , onPointerLeave :: Event
       , onPointerMove :: Event
       , onPointerOut :: Event
       , onPointerOver :: Event
       , onPointerUp :: Event
       , onScroll :: Event
       , onTouchCancel :: TouchEvent
       , onTouchEnd :: TouchEvent
       , onTouchEnter :: TouchEvent
       , onTouchLeave :: TouchEvent
       , onTouchMove :: TouchEvent
       , onTouchStart :: TouchEvent
       , onTransitionEnd :: Event
       , spellcheck :: Boolean
       , style :: String
       , tabIndex :: Int
       , title :: String
       )
       t19
    )
  -> Array (HTML t20 t19) -> HTML t20 t19
container :: forall t19 t20.
  Array
    (IProp
       ( "class" :: String
       , accessKey :: String
       , contentEditable :: Boolean
       , dir :: DirValue
       , draggable :: Boolean
       , gotPointerCapture :: Event
       , hidden :: Boolean
       , id :: String
       , lang :: String
       , lostPointerCapture :: Event
       , onBlur :: FocusEvent
       , onClick :: MouseEvent
       , onContextMenu :: Event
       , onCopy :: ClipboardEvent
       , onCut :: ClipboardEvent
       , onDoubleClick :: MouseEvent
       , onDrag :: DragEvent
       , onDragEnd :: DragEvent
       , onDragEnter :: DragEvent
       , onDragExit :: DragEvent
       , onDragLeave :: DragEvent
       , onDragOver :: DragEvent
       , onDragStart :: DragEvent
       , onDrop :: DragEvent
       , onFocus :: FocusEvent
       , onFocusIn :: FocusEvent
       , onFocusOut :: FocusEvent
       , onKeyDown :: KeyboardEvent
       , onKeyPress :: KeyboardEvent
       , onKeyUp :: KeyboardEvent
       , onMouseDown :: MouseEvent
       , onMouseEnter :: MouseEvent
       , onMouseLeave :: MouseEvent
       , onMouseMove :: MouseEvent
       , onMouseOut :: MouseEvent
       , onMouseOver :: MouseEvent
       , onMouseUp :: MouseEvent
       , onPaste :: ClipboardEvent
       , onPointerCancel :: Event
       , onPointerDown :: Event
       , onPointerEnter :: Event
       , onPointerLeave :: Event
       , onPointerMove :: Event
       , onPointerOut :: Event
       , onPointerOver :: Event
       , onPointerUp :: Event
       , onScroll :: Event
       , onTouchCancel :: TouchEvent
       , onTouchEnd :: TouchEvent
       , onTouchEnter :: TouchEvent
       , onTouchLeave :: TouchEvent
       , onTouchMove :: TouchEvent
       , onTouchStart :: TouchEvent
       , onTransitionEnd :: Event
       , spellcheck :: Boolean
       , style :: String
       , tabIndex :: Int
       , title :: String
       )
       t19
    )
  -> Array (HTML t20 t19) -> HTML t20 t19
container attrs = H.div (P.class_ B.container : attrs)



container_ :: Array (HTML _ _) -> HTML _ _
container_ = container []
