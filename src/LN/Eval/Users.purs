module LN.Eval.Users (
  eval_GetUsers,
  eval_GetUser,
  eval_GetUsers_MergeMap_ByUser,
  eval_GetUsers_MergeMap_ByUserId
) where



import Halogen                       (gets, modify)
import Data.Array                    (nub, filter)
import Data.Either                   (Either(..))
import Data.Map                      as M
import Data.Maybe                    (Maybe(..))
import Data.Tuple                    (Tuple(..))
import Optic.Core                    ((^.), (..))
import Prelude                       (bind, pure, not, map, discard, ($), (==))

import LN.ArrayList                  (arrayToList)
import LN.Api                        (getUsersCount' , getUserSanitizedPacks
                                     , getUserSanitizedPacks_ByUsersIds')
import LN.Helpers.Api                (rd)
import LN.Api.String                 as ApiS
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.Types                (Input(..))
import LN.State.PageInfo             (runPageInfo)
import LN.T



-- eval_GetUsers :: Partial => EvalEff
{-
eval_GetUsers :: forall t117 t118 t124 t131.
  Partial => Bind t117 => MonadState
                            { usersPageInfo :: { currentPage :: Int
                                               , resultsPerPage :: Int
                                               , totalResults :: Int
                                               , totalPages :: Int
                                               , sortOrder :: SortOrderBy
                                               , order :: OrderBy
                                               }
                            | t124
                            }
                            t117
                           => MonadAff
                                ( ajax :: AJAX
                                , console :: CONSOLE
                                | t131
                                )
                                t117
                               => (Input t118 -> t117 t118) -> Input t118 -> t117 t118
                               -}
eval_GetUsers eval (GetUsers next) = do

  page_info <- gets _.usersPageInfo

  e_count <- rd getUsersCount'
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetUsers::getUsersCount'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_{ usersPageInfo = new_page_info.pageInfo })

      e_users <- rd $ getUserSanitizedPacks new_page_info.params

      case e_users of
        Left err                                      -> eval (AddErrorApi "eval_GetUsers::getUserSanitizedPacks" err next)
        Right (UserSanitizedPackResponses user_packs) -> do

          let
            users_map = idmapFrom (\(UserSanitizedPackResponse p) -> p.userId) user_packs.userSanitizedPackResponses

          -- TODO FIXME: merge this with pre-existing users? union? correct?
          -- BUG: this union causes the page not to update... add these users to usersMap, but users itself needs to be fresh
          -- modify (\st -> st{ users = M.union st.users users_map })
          modify (\st -> st { users = users_map })
          pure next



eval_GetUser :: Partial => EvalEff
eval_GetUser eval (GetUser user_nick next) = do

  modify (_{ currentUser = Nothing })

  e_user <- rd $ ApiS.getUserSanitizedPack' user_nick
  case e_user of
      Left err -> pure next
      Right user -> do
        modify (_{ currentUser = Just user })
        pure next



-- | Takes an array of sanitized users, and pulls down any of them that
-- don't already exist in the current st.usersMap.
--
eval_GetUsers_MergeMap_ByUser :: Partial => EvalEff
eval_GetUsers_MergeMap_ByUser eval (GetUsers_MergeMap_ByUser users next) = do

  let
    users_ids = map (\user -> user ^. _UserSanitizedResponse .. id_) users

  eval_GetUsers_MergeMap_ByUserId eval (GetUsers_MergeMap_ByUserId users_ids next)




-- | Takes an array of users ids, and pulls down any of them that
-- don't already exist in the current st.usersMap.
--
eval_GetUsers_MergeMap_ByUserId :: Partial => EvalEff
eval_GetUsers_MergeMap_ByUserId eval (GetUsers_MergeMap_ByUserId users_ids next) = do

  usersMap <- gets _.usersMap

  let
    users_ids_not_in_map =
      filter (\user_id -> not $ M.member user_id usersMap)
      $ nub users_ids

  if users_ids_not_in_map == []
     then pure next
     else do
       e_result <- rd $ getUserSanitizedPacks_ByUsersIds' users_ids_not_in_map

       case e_result of
            Left err -> pure next
            Right (UserSanitizedPackResponses result) -> do
              let
               newUsersMap =
                 M.fromFoldable
                 $ arrayToList
                 $ map (\user -> Tuple (user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. id_) user) result.userSanitizedPackResponses

              modify (_{ usersMap = (M.union newUsersMap usersMap) })
              pure next
