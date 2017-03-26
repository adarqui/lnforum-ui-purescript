module LN.Eval.Goto (
  eval_Goto
) where



import Control.Monad.Aff.Free   (fromAff)
import Data.Ebyam               (ebyam)
import Data.Functor             (($>))
import Data.Map                 as M
import Data.Maybe               (Maybe(..), maybe)
import Halogen                  (get, gets, modify)
import Optic.Core               ((^.),(..))
import Prelude                  (show, bind, pure, unit, id, (==), (/=), (<), ($))

import Purescript.Api.Helpers   (qp)

import LN.Component.Types       (EvalEff)
import LN.Input.Types
import LN.Input.Organization    as Organization
import LN.Input.Team            as Team
import LN.Input.TeamMember      as TeamMember
import LN.Input.Forum           as Forum
import LN.Input.Board           as Board
import LN.Input.Thread          as Thread
import LN.Input.ThreadPost      as ThreadPost
import LN.Internal.Leuron       (defaultLeuronRequest, leuronToTyLeuron)
import LN.Internal.Resource     (defaultResourceRequest, resourceTypeToTyResourceType)
import LN.Router.Link           (updateUrl)
import LN.Router.Types          (Routes(..), CRUD(..))
import LN.Router.Class.Params   (lookupParam)
import LN.State.PageInfo        (defaultPageInfo_Threads
                                ,defaultPageInfo_ThreadPosts
                                ,defaultPageInfo_Resources
                                ,defaultPageInfo_Leurons
                                ,defaultPageInfo_Users)
import LN.State.Organization    (defaultOrganizationRequestState)
import LN.State.Forum           (defaultForumRequestState)
import LN.State.Board           (defaultBoardRequestState)
import LN.State.Thread          (defaultThreadRequestState)
import LN.State.ThreadPost      (defaultThreadPostRequestState)
import LN.State.Leuron          (defaultLeuronRequestState, leuronRequestStateFromLeuronData)
import LN.State.Resource        (defaultResourceRequestState)
import LN.T




eval_Goto :: EvalEff
eval_Goto eval (Goto route next) = do

  modify (_ { currentPage = route })
  fromAff $ updateUrl route

  st <- get

  case route of



    Me          -> eval (GetMe next) $> unit



    Errors      -> pure unit



    -- Organizations
    --
    (Organizations Index params) -> do
      eval (cOrganizationAct Organization.Gets next)
      pure unit

    (Organizations New params) -> do
      modify (_{ currentOrganizationRequest = Just defaultOrganizationRequest, currentOrganizationRequestSt = Just defaultOrganizationRequestState })
      pure unit

    (Organizations (Edit org_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      m_pack <- gets _.currentOrganization
      case m_pack of
           Nothing                              -> pure unit
           Just (OrganizationPackResponse pack) -> do
             m_req_st <- gets _.currentOrganizationRequestSt
             let
               org    = pack.organization ^. _OrganizationResponse
               req_st = maybe defaultOrganizationRequestState id m_req_st
             modify (_{ currentOrganizationRequest = Just $ organizationResponseToOrganizationRequest pack.organization, currentOrganizationRequestSt = Just req_st })
             pure unit

    (Organizations (Delete org_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      m_pack <- gets _.currentOrganization
      case m_pack of
           Nothing                          -> pure unit
           Just (OrganizationPackResponse pack) -> do
             modify (_{ currentOrganizationRequest = Just $ organizationResponseToOrganizationRequest pack.organization })
             pure unit

    (Organizations (Show org_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct Forum.Gets_ByCurrentOrganization next)
      pure unit



    -- Organizations Forums
    --
    (OrganizationsForums org_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct Forum.Gets_ByCurrentOrganization next)
      pure unit

    (OrganizationsForums org_name New params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      modify (_{ currentForumRequest = Just defaultForumRequest, currentForumRequestSt = Just defaultForumRequestState })
      pure unit

    (OrganizationsForums org_name (Edit forum_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      m_pack <- gets _.currentForum
      case m_pack of
           Nothing                              -> pure unit
           Just (ForumPackResponse pack) -> do
             m_o_st <- gets _.currentForumRequestSt
             let
               org  = pack.forum ^. _ForumResponse
               o_st = maybe defaultForumRequestState id m_o_st
             modify (_{ currentForumRequest = Just $ forumResponseToForumRequest pack.forum, currentForumRequestSt = Just o_st })
             pure unit

    (OrganizationsForums org_name (Delete forum_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      m_pack <- gets _.currentForum
      case m_pack of
           Nothing                          -> pure unit
           Just (ForumPackResponse pack) -> do
             modify (_{ currentForumRequest = Just $ forumResponseToForumRequest pack.forum })
             pure unit

    (OrganizationsForums org_name (Show forum_name) params) -> do
      eval (Goto (OrganizationsForumsBoards org_name forum_name Index params) next)
      pure unit



    -- Organizations Forums Boards
    --
    (OrganizationsForumsBoards org_name forum_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct Board.Gets_ByCurrentForum next)
      -- Recent Posts
      eval (cForumAct Forum.GetRecentPosts_ByCurrentForum next)
      -- Messages of the week
      eval (cForumAct Forum.GetMessagesOfTheWeek_ByCurrentForum next)
      pure unit

    (OrganizationsForumsBoards org_name forum_name New params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      modify (_{ currentBoardRequest = Just defaultBoardRequest, currentBoardRequestSt = Just defaultBoardRequestState })
      pure unit

    (OrganizationsForumsBoards org_name forum_name (Edit board_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      m_pack <- gets _.currentBoard
      case m_pack of
           Nothing                              -> pure unit
           Just (BoardPackResponse pack) -> do
             m_o_st <- gets _.currentBoardRequestSt
             let
               org  = pack.board ^. _BoardResponse
               o_st = maybe defaultBoardRequestState id m_o_st
             modify (_{ currentBoardRequest = Just $ boardResponseToBoardRequest pack.board, currentBoardRequestSt = Just o_st })
             pure unit

    (OrganizationsForumsBoards org_name forum_name (Delete board_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      m_pack <- gets _.currentBoard
      case m_pack of
           Nothing                          -> pure unit
           Just (BoardPackResponse pack) -> do
             modify (_{ currentBoardRequest = Just $ boardResponseToBoardRequest pack.board })
             pure unit

    (OrganizationsForumsBoards org_name forum_name (Show board_name) params) -> do
      eval (Goto (OrganizationsForumsBoardsThreads org_name forum_name board_name Index params) next)
      pure unit



    -- Organizations Forums Boards Threads
    --
    (OrganizationsForumsBoardsThreads org_name forum_name board_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)

      let offset     = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Threads.currentPage (\(Offset v) -> v)
      let order      = ebyam (lookupParam ParamTag_Order params) defaultPageInfo_Threads.order (\(Order v) -> v)
      let sort_order = ebyam (lookupParam ParamTag_SortOrder params) defaultPageInfo_Threads.sortOrder (\(SortOrder v) -> v)

      pageInfo <- gets _.threadsPageInfo
      modify (_{ threadsPageInfo = pageInfo { currentPage = offset, order = order, sortOrder = sort_order } })

      eval (cThreadAct Thread.Gets_ByCurrentBoard next)
      pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name New params) -> do
      modify (_{ currentThreadRequest = Just defaultThreadRequest, currentThreadRequestSt = Just defaultThreadRequestState })
      pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name (Edit thread_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      eval (cThreadAct (Thread.GetSid_ByCurrentBoard thread_name) next)
      m_pack <- gets _.currentThread
      case m_pack of
           Nothing                              -> pure unit
           Just (ThreadPackResponse pack) -> do
             m_o_st <- gets _.currentThreadRequestSt
             let
               org  = pack.thread ^. _ThreadResponse
               o_st = maybe defaultThreadRequestState id m_o_st
             modify (_{ currentThreadRequest = Just $ threadResponseToThreadRequest pack.thread, currentThreadRequestSt = Just o_st })
             pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name (Delete thread_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      eval (cThreadAct (Thread.GetSid_ByCurrentBoard thread_name) next)
      m_pack <- gets _.currentThread
      case m_pack of
           Nothing                          -> pure unit
           Just (ThreadPackResponse pack) -> do
             modify (_{ currentThreadRequest = Just $ threadResponseToThreadRequest pack.thread })
             pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread_name) params) -> do
      eval (Goto (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name Index params) next)
      pure unit



    -- Organizations Forums Boards Threads Posts
    --
    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      eval (cThreadAct (Thread.GetSid_ByCurrentBoard thread_name) next)

      pageInfo <- gets _.threadPostsPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_ThreadPosts.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)

      modify (_{ currentThreadPostRequest = Just defaultThreadPostRequest, currentThreadPostRequestSt = Just defaultThreadPostRequestState, threadPostsPageInfo = pageInfo { currentPage = offset } })

      eval (cThreadPostAct ThreadPost.Gets_ByCurrentThread next)
      pure unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name New params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      eval (cThreadAct (Thread.GetSid_ByCurrentBoard thread_name) next)
      modify (_{ currentThreadPostRequest = Just defaultThreadPostRequest, currentThreadPostRequestSt = Just defaultThreadPostRequestState })
      pure unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (EditI post_id) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      eval (cThreadAct (Thread.GetSid_ByCurrentBoard thread_name) next)
      eval (cThreadPostAct (ThreadPost.GetId post_id) next)
      m_pack <- gets _.currentThreadPost
      case m_pack of
           Nothing                              -> pure unit
           Just (ThreadPostPackResponse pack) -> do
             m_st <- gets _.currentThreadPostRequestSt
             let
               _st = maybe defaultThreadPostRequestState id m_st
             modify (_{ currentThreadPostRequest = Just $ threadPostResponseToThreadPostRequest pack.threadPost, currentThreadPostRequestSt = Just _st })
             pure unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (DeleteI post_id) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      eval (cThreadAct (Thread.GetSid_ByCurrentBoard thread_name) next)
      eval (cThreadPostAct (ThreadPost.GetId post_id) next)
      m_pack <- gets _.currentThreadPost
      case m_pack of
           Nothing                          -> pure unit
           Just (ThreadPostPackResponse pack) -> do
             modify (_{ currentThreadPostRequest = Just $ threadPostResponseToThreadPostRequest pack.threadPost })
             pure unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (ShowI post_id) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.GetSid_ByCurrentOrganization forum_name) next)
      eval (cBoardAct (Board.GetSid_ByCurrentForum board_name) next)
      eval (cThreadAct (Thread.GetSid_ByCurrentBoard thread_name) next)

      pageInfo <- gets _.threadPostsPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_ThreadPosts.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)

      modify (_{ currentThreadPostRequest = Just defaultThreadPostRequest, currentThreadPostRequestSt = Just defaultThreadPostRequestState, threadPostsPageInfo = pageInfo{currentPage = offset} })

      eval (cThreadPostAct (ThreadPost.Gets_ByCurrentThread_And_ThreadPostId post_id) next)
      pure unit



    -- Organizations MembersOnly
    --
    (OrganizationsMembersOnly org_name) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      pure unit



    -- Organizations Membership
    --
    (OrganizationsMembership org_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cTeamAct Team.Gets_ByCurrentOrganization next)
      pure unit

    (OrganizationsMembership org_name DeleteZ params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      pure unit



    -- Teams
    --
    (OrganizationsTeams org_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cTeamAct Team.Gets_ByCurrentOrganization next)
      pure unit

    (OrganizationsTeams org_name (Show team_name) params) -> do
      eval (Goto (OrganizationsTeamsMembers org_name team_name Index params) next)
      pure unit



    -- Team Members
    --
    (OrganizationsTeamsMembers org_name team_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cTeamAct (Team.GetSid_ByCurrentOrganization team_name) next)
      eval (cTeamMemberAct TeamMember.Gets_ByCurrentTeam next)
      pure unit

    (OrganizationsTeamsMembers org_name team_name (ShowI team_member_id) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cTeamAct (Team.GetSid_ByCurrentOrganization team_name) next)
      eval (cTeamMemberAct (TeamMember.GetId team_member_id) next)
      pure unit



    -- Resources
    --
    (Resources Index params) -> do

      pageInfo <- gets _.resourcesPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Resources.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ resourcesPageInfo = pageInfo { currentPage = offset } })

      eval (GetResources next) $> unit



    (Resources New params) -> do
      modify (_{ currentResourceRequest = Just defaultResourceRequest, currentResourceRequestSt = Just defaultResourceRequestState })
      pure unit

    (Resources (EditI resource_id) params)   -> do
      eval (GetResourceId resource_id next)
      m_pack <- gets _.currentResource
      case m_pack of
           Nothing                          -> pure unit
           Just (ResourcePackResponse pack) -> do
             -- TODO FIXME: St's TyResourceType needs to match source
             let
               resource = pack.resource ^. _ResourceResponse
               rst      = defaultResourceRequestState { source = resourceTypeToTyResourceType resource.source }
             modify (_{ currentResourceRequest = Just $ resourceResponseToResourceRequest pack.resource, currentResourceRequestSt = Just rst })
             pure unit

    (Resources (DeleteI resource_id) params) -> do
      eval (GetResourceId resource_id next)
      m_pack <- gets _.currentResource
      case m_pack of
           Nothing                          -> pure unit
           Just (ResourcePackResponse pack) -> do
             modify (_{ currentResourceRequest = Just $ resourceResponseToResourceRequest pack.resource })
             pure unit


    (Resources (ShowI resource_id) params)   -> eval (GetResourceId resource_id next) $> unit



    (ResourcesLeurons resource_id Index params) -> do

      pageInfo <- gets _.leuronsPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Leurons.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ leuronsPageInfo = pageInfo { currentPage = offset } })

      eval (GetLeurons next) $> unit

    (ResourcesLeurons resource_id New params) -> do
      -- Important: don't over-write leuron request state.. we want to hold on to that info to make our lives easier
      -- when adding leurons fast
      eval (GetResourceId resource_id next) -- TODO FIXME
      lst <- gets _.currentLeuronRequestSt
      modify (_{ currentLeuronRequest = Just defaultLeuronRequest, currentLeuronRequestSt = Just $ maybe defaultLeuronRequestState id lst })
      pure unit

    (ResourcesLeurons resource_id (EditI leuron_id) params)   -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetLeuronId leuron_id next)
      m_pack <- gets _.currentLeuron
      case m_pack of
           Nothing                          -> pure unit
           Just (LeuronPackResponse pack) -> do
             -- TODO FIXME: St's TyLeuronType needs to match source
             m_lst <- gets _.currentLeuronRequestSt
             let
               leuron = pack.leuron ^. _LeuronResponse
               lst    = leuronRequestStateFromLeuronData leuron.dataP (maybe defaultLeuronRequestState id m_lst)
             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron, currentLeuronRequestSt = Just lst })
             pure unit

    (ResourcesLeurons resource_id (DeleteI leuron_id) params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetLeuronId leuron_id next)
      m_pack <- gets _.currentLeuron
      case m_pack of
           Nothing                          -> pure unit
           Just (LeuronPackResponse pack) -> do
             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron })
             pure unit

    (ResourcesLeurons resource_id (ShowI leuron_id) params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetLeuronId leuron_id next) $> unit



    (ResourcesSiftLeurons resource_id params) -> do
      pure unit

    (ResourcesSiftLeuronsLinear resource_id (ShowI offset) params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetResourceLeuronLinear resource_id offset next)
      pure unit

    (ResourcesSiftLeuronsRandom resource_id params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetResourceLeuronRandom resource_id next)
      pure unit




--    (Leurons Index params) -> do
--      let m_offset = M.lookup (\(Tuple k v) -> k == "offset") params
--      maybe
--        (pure unit)
--        (\(Tuple k offset) -> do
--          pageInfo <- gets _.leuronsPageInfo
--          modify (_{ leuronsPageInfo = pageInfo { currentPage = maybe 1 id (fromString offset) } })
--          pure unit)
--        m_offset
--      eval (GetLeurons next) $> unit
--
--    (Leurons New params) -> do
--      -- Important: don't over-write leuron request state.. we want to hold on to that info to make our lives easier
--      -- when adding leurons fast
--      lst <- gets _.currentLeuronRequestSt
--      modify (_{ currentLeuronRequest = Just defaultLeuronRequest, currentLeuronRequestSt = Just $ maybe defaultLeuronRequestState id lst })
--      pure unit
--
--    (Leurons (EditI leuron_id) params)   -> do
--      eval (GetLeuronId leuron_id next)
--      m_pack <- gets _.currentLeuron
--      case m_pack of
--           Nothing                          -> pure unit
--           Just (LeuronPackResponse pack) -> do
--             -- TODO FIXME: St's TyLeuronType needs to match source
--             let
--               leuron = pack.leuron ^. _LeuronResponse
--               lst    = defaultLeuronRequestState { ty = leuronToTyLeuron leuron.dataP }
--             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron, currentLeuronRequestSt = Just lst })
--             pure unit
--
--    (Leurons (DeleteI leuron_id) params) -> do
--      eval (GetLeuronId leuron_id next)
--      m_pack <- gets _.currentLeuron
--      case m_pack of
--           Nothing                          -> pure unit
--           Just (LeuronPackResponse pack) -> do
--             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron })
--             pure unit
--
--    (Leurons (ShowI leuron_id) params) -> eval (GetLeuronId leuron_id next) $> unit
--


    (Users Index params) -> do
      pageInfo <- gets _.usersPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Users.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ usersPageInfo = pageInfo { currentPage = offset } })
      eval (GetUsers next) $> unit

    (Users (Show user_nick) params) -> eval (GetUser user_nick next) $> unit



    (UsersProfile user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersSettings user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersPMs user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersThreads user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersThreadPosts user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersWorkouts user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersResources user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersLeurons user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersLikes user_nick params) -> eval (GetUser user_nick next) $> unit



    _           -> pure unit



  pure next
