module LN.Component where


import Data.NaturalTransformation
import Data.Maybe
import Prelude                     (Void, Unit, unit, pure, bind, const, ($))
import Halogen
import Halogen.HTML as HH
import Halogen.HTML            as H
import Halogen.HTML.Properties as P

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
import LN.Eval.Resources
import LN.Eval.Leurons
import LN.Eval.Socket
import LN.Eval.Nop

import LN.Router.Types


-- ui :: forall eff. {-Partial =>-} Component State Input (LNEff eff)
ui :: forall eff. Partial => State -> Component HH.HTML Input Unit Void eff
ui st =
  component {
    initialState: const $ st,
    render,
    eval,
    receiver: const Nothing
  }

  where

  render :: State -> ComponentHTML Input
  render state = H.p_ [H.text "hello"]
--    L.defaultLayout state
--      [ renderView About state ]
--      [ renderView state.currentPage state ]

  eval :: Partial => Input ~> ComponentDSL State Input Void eff
  eval ev = case ev of
    Nop next -> do
      -- pure next
      eval_Nop''
      pure next

{-
ui :: State -> ComponentHTML Input
ui = component {render, eval}

  where
  render state =
    L.defaultLayout state
      [ renderView state.currentPage state
      ]

  eval :: CompEff
  -}

{-
  eval z@(Goto _ _)                                   = eval_Goto eval z
--  eval z@(Goto route next)                              = eval_Goto eval route next

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
  eval z@(CompProfile _ _)                            = eval_Profile eval z
  eval z@(CompResource _ _)                           = eval_Resource eval z
  eval z@(CompLeuron _ _)                             = eval_Leuron eval z
  eval z@(CompOrderBy _ _)                            = eval_OrderBy eval z

  eval z@(Nop _)                                      = eval_Nop eval z
  -}
