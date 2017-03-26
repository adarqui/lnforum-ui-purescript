module LN.Component where


import Halogen                     hiding (set)
import Prelude                     (pure)

import LN.Layout                   as L
import LN.Component.Types          (LNEff, CompEff)
import LN.Input.Types              (Input(..))
import LN.State.Types              (State)
import LN.View
import LN.Eval.ArrayString
import LN.Eval.Goto
import LN.Eval.Errors
import LN.Eval.Me
import LN.Eval.OrderBy
import LN.Eval.Profile
import LN.Eval.Users
import LN.Eval.Memberships
import LN.Eval.Resources
import LN.Eval.Leurons
import LN.Eval.Socket
import LN.Eval.Nop



ui :: forall eff. {-Partial =>-} Component State Input (LNEff eff)
ui = component {render, eval}

  where
  render state =
    L.defaultLayout state
      [ renderView state.currentPage state
      ]

  eval :: CompEff
--  eval :: Eval Input State Input (LNEff eff)

  eval z@(Goto _ _)                                   = eval_Goto eval z

  eval z@(AddError _ _ _)                             = eval_AddError eval z
  eval z@(AddErrorF _ _ _)                            = eval_AddErrorF eval z
  eval z@(AddErrorApi _ _ _)                          = eval_AddErrorApi eval z
  eval z@(DelError _ _)                               = eval_DelError eval z
  eval z@(ClearErrors _)                              = eval_ClearErrors eval z

  eval z@(GetMe _)                                    = eval_GetMe eval z

  eval z@(GetUsers _)                                 = eval_GetUsers eval z

  eval z@(GetUser _ _)                                = eval_GetUser eval z

  eval z@(GetUsers_MergeMap_ByUser _ _)               = eval_GetUsers_MergeMap_ByUser eval z
  eval z@(GetUsers_MergeMap_ByUserId _ _)             = eval_GetUsers_MergeMap_ByUserId eval z

  eval (GetThreadPostLikes next)                      = pure next

  eval z@(GetResources _)                             = eval_GetResources eval z
  eval z@(GetResourceId _ _)                          = eval_GetResourceId eval z

  eval z@(GetResourcesLeurons _ _)                    = eval_GetResources eval z
  eval z@(GetResourceLeuronLinear _ _ _)              = eval_GetResourceLeuronLinear eval z
  eval z@(GetResourceLeuronRandom _ _)                = eval_GetResourceLeuronRandom eval z
  eval z@(GetResourcesSiftLeurons _ _)                = eval_GetResourcesSiftLeurons eval z

  eval z@(GetLeurons _)                               = eval_GetLeurons eval z
  eval z@(GetLeuronId _ _)                            = eval_GetLeuronId eval z
  eval z@(GetLeuronRandom _)                          = eval_GetLeuronRandom eval z

  eval z@(ConnectSocket _)                            = eval_ConnectSocket eval z

  -- Components

  eval z@(CompArrayString _ _)                        = eval_ArrayString eval z
  eval z@(CompOrganization _ _)                       = eval_Organization eval z
  eval z@(CompTeam _ _)                               = eval_Team eval z
  eval z@(CompTeamMember _ _)                         = eval_TeamMember eval z
  eval z@(CompMembership _ _)                         = eval_Membership eval z
  eval z@(CompForum _ _)                              = eval_Forum eval z
  eval z@(CompBoard _ _)                              = eval_Board eval z
  eval z@(CompThread _ _)                             = eval_Thread eval z
  eval z@(CompThreadPost _ _)                         = eval_ThreadPost eval z
  eval z@(CompProfile _ _)                            = eval_Profile eval z
  eval z@(CompResource _ _)                           = eval_Resource eval z
  eval z@(CompLeuron _ _)                             = eval_Leuron eval z
  eval z@(CompPm _ _)                                 = eval_Pm eval z
  eval z@(CompPmIn _ _)                               = eval_PmIn eval z
  eval z@(CompPmOut _ _)                              = eval_PmOut eval z
  eval z@(CompLike _ _)                               = eval_Like eval z
  eval z@(CompStar _ _)                               = eval_Star eval z
  eval z@(CompOrderBy _ _)                            = eval_OrderBy eval z

  eval z@(Nop _)                                      = eval_Nop eval z
