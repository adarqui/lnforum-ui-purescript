module LN.Eval.Forums (
  eval_GetForums,
  eval_GetForumsForOrg,
  eval_Forum
) where



import Data.Array                      (head, deleteAt, modifyAt, nub)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, bind, pure, ($), (<>))

import LN.Api                          (rd, getForumPacks_ByOrganizationName', getForumPack', postForum_ByOrganizationId', putForum')
import LN.Component.Types              (EvalEff)
import LN.Input.Forum                  (InputForum(..), Forum_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Helpers.Map                  (idmapFrom)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.State.Loading                (l_currentForum)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.T.Internal.Convert           (forumResponseToForumRequest)
import LN.T                            ( ForumPackResponses(..), ForumPackResponse(..)
                                       , ForumResponse(..)
                                       , ForumRequest(..)
                                       , _ForumRequest, displayName_, description_, icon_, tags_, visibility_, guard_
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_, name_)



eval_GetForums :: EvalEff
eval_GetForums eval (GetForums next) = pure next



eval_GetForumsForOrg :: EvalEff
eval_GetForumsForOrg eval (GetForumsForOrg org_name next) = do

  modify (_{ forums = (M.empty :: M.Map Int ForumPackResponse) })

  e_forum_packs <- rd $ getForumPacks_ByOrganizationName' org_name
  case e_forum_packs of
    Left err                               -> eval (AddErrorApi "eval_GetForumsForOrg::getForumPacks_ByOrgName'" err next)
    Right (ForumPackResponses forum_packs) -> do

      let
        forums_map = idmapFrom (\(ForumPackResponse pack) -> pack.forumId) forum_packs.forumPackResponses

      modify (_{ forums = forums_map })
      pure next



eval_Forum :: EvalEff
eval_Forum eval (CompForum sub next) = do

  org_pack <- gets _.currentOrganization
  let org_name = maybe "unknown" (\org -> org ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_) org_pack

  case sub of
    InputForum_Mod q -> do
      case q of
        SetDisplayName name -> mod $ set (\req -> _ForumRequest .. displayName_ .~ name $ req)

        SetDescription s    -> mod $ set (\req -> _ForumRequest .. description_ .~ Just s $ req)
        RemoveDescription   -> mod $ set (\req -> _ForumRequest .. description_ .~ Nothing $ req)

        SetIcon s           -> mod $ set (\req -> _ForumRequest .. icon_ .~ Just s $ req)
        RemoveIcon          -> mod $ set (\req -> _ForumRequest .. icon_ .~ Nothing $ req)

        Create org_id       -> do

          m_req <- gets _.currentForumRequest

          case m_req of

               Just req -> do

                 e_forum <- rd $ postForum_ByOrganizationId' org_id req

                 case e_forum of
                      Left err                    -> eval (AddErrorApi "eval_Forum(Create)::postForum'" err next)
                      Right (ForumResponse forum) -> eval (Goto (OrganizationsForums org_name (Show forum.name) []) next)

               _        -> eval (AddError "eval_Forum(Create)" "Forum request doesn't exist" next)


        EditP forum_id -> do

          m_req <- gets _.currentForumRequest

          case m_req of
               Nothing  -> eval (AddError "eval_Forum(Edit)" "Forum request doesn't exist" next)
               Just req -> do

                 e_org <- rd $ putForum' forum_id req

                 case e_org of
                      Left err  -> eval (AddErrorApi "eval_Forum(Edit)::putForum" err next)
                      Right org -> do

                        modify (\st->st{ currentForumRequest = Just $ forumResponseToForumRequest org })
                        pure next

    _   -> pure next

 where
 append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
 append Nothing a    = Just [a]
 append (Just arr) a = Just $ nub $ arr <> [a]
 set v req           = Just (v req)
 mod new             = modify (\st->st{ currentForumRequest = maybe Nothing new st.currentForumRequest }) $> next
