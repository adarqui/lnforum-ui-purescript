module LN.Access (
  permissionsHTML,
  permissionsHTML',
  permissionsMatchHTML,
  permissionsMatchCreateHTML,
  permissionsMatchReadHTML,
  permissionsMatchUpdateHTML,
  permissionsMatchDeleteHTML,
  permissionsMatchExecuteHTML,
  unitDiv,
  permCreateEmpty,
  permReadEmpty,
  permUpdateEmpty,
  permDeleteEmpty,
  permExecuteEmpty,
  ifte_Self,
  ifte_NotSelf,
  self,
  notSelf
) where



-- import Data.Array                      (elem)
import Data.Foldable (elem)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (Unit, unit, (==), (/=))

import LN.T




permissionsHTML
  :: Permissions
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> HTML _ _
permissionsHTML perms create_cb no_create_cb read_cb no_read_cb update_cb no_update_cb delete_cb no_delete_cb execute_cb no_execute_cb =
  H.div_
    [
      if Perm_Create `elem` perms then (create_cb unit) else (no_create_cb unit),
      if Perm_Read `elem` perms then (read_cb unit) else (no_read_cb unit),
      if Perm_Update `elem` perms then (update_cb unit) else (no_update_cb unit),
      if Perm_Delete `elem` perms then (delete_cb unit) else (no_delete_cb unit),
      if Perm_Execute `elem` perms then (execute_cb unit) else (no_execute_cb unit)
    ]



permissionsHTML'
  :: Permissions
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> HTML _ _
permissionsHTML' perms create_cb read_cb update_cb delete_cb execute_cb =
  permissionsHTML perms create_cb unitDiv read_cb unitDiv update_cb unitDiv delete_cb unitDiv execute_cb unitDiv



unitDiv :: Unit -> HTML _ _
unitDiv _ = H.div_ []



permCreateEmpty  = unitDiv
permReadEmpty    = unitDiv
permUpdateEmpty  = unitDiv
permDeleteEmpty  = unitDiv
permExecuteEmpty = unitDiv



permissionsMatchCreateHTML :: Permissions -> (Unit -> HTML _ _) -> (Unit -> HTML _ _) -> HTML _ _
permissionsMatchCreateHTML = permissionsMatchHTML Perm_Create



permissionsMatchReadHTML :: Permissions -> (Unit -> HTML _ _) -> (Unit -> HTML _ _) -> HTML _ _
permissionsMatchReadHTML = permissionsMatchHTML Perm_Read



permissionsMatchUpdateHTML :: Permissions -> (Unit -> HTML _ _) -> (Unit -> HTML _ _) -> HTML _ _
permissionsMatchUpdateHTML = permissionsMatchHTML Perm_Update



permissionsMatchDeleteHTML :: Permissions -> (Unit -> HTML _ _) -> (Unit -> HTML _ _) -> HTML _ _
permissionsMatchDeleteHTML = permissionsMatchHTML Perm_Delete



permissionsMatchExecuteHTML :: Permissions -> (Unit -> HTML _ _) -> (Unit -> HTML _ _) -> HTML _ _
permissionsMatchExecuteHTML = permissionsMatchHTML Perm_Execute



permissionsMatchHTML
  :: Permission
  -> Permissions
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> HTML _ _
permissionsMatchHTML perm_to_match permissions match_cb no_match_cb =
  if perm_to_match `elem` permissions then (match_cb unit) else (no_match_cb unit)






ifte_Self :: forall a. Int -> Int -> a -> a -> a
ifte_Self my_id questionable_id t e =
  if my_id == questionable_id
     then t
     else e



ifte_NotSelf :: forall a. Int -> Int -> a -> a -> a
ifte_NotSelf my_id questionable_id t e =
  if my_id /= questionable_id
     then t
     else e



self :: Int -> Int -> Boolean
self my_id questionable_id = my_id == questionable_id



notSelf :: Int -> Int -> Boolean
notSelf my_id questionable_id = my_id /= questionable_id
