module LN.Component where


import Data.NaturalTransformation
import Data.Maybe
import Prelude                     (Void, Unit, unit, pure, bind, const, ($))
import Halogen
import Halogen.HTML as HH
import Halogen.HTML            as H
import Halogen.HTML.Properties as P

import LN.Layout                   as L
import LN.Component.Types          (LNEff, CompEff, UICompEff)
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
import LN.Eval.Buckets
import LN.Eval.BucketRounds
import LN.Eval.Socket
import LN.Eval.Nop

import LN.Router.Types



ui :: forall eff. Partial => State -> UICompEff
ui st =
  component {
    initialState: const $ st,
    render,
    eval,
    receiver: const Nothing
  }

  where

  render :: State -> ComponentHTML Input
  render state =
    L.defaultLayout state
      [ renderView state.currentPage state ]

  eval :: Partial => CompEff
  eval ev = case ev of
    Goto _ _                                   -> eval_Goto eval ev

    AddError _ _ _                             -> eval_AddError eval ev
    AddErrorF _ _ _                            -> eval_AddErrorF eval ev
    AddErrorApi _ _ _                          -> eval_AddErrorApi eval ev
    DelError _ _                               -> eval_DelError eval ev
    ClearErrors _                              -> eval_ClearErrors eval ev

    GetMe _                                    -> eval_GetMe eval ev

    GetUsers _                                 -> eval_GetUsers eval ev

    GetUser _ _                                -> eval_GetUser eval ev

    GetUsers_MergeMap_ByUser _ _               -> eval_GetUsers_MergeMap_ByUser eval ev
    GetUsers_MergeMap_ByUserId _ _             -> eval_GetUsers_MergeMap_ByUserId eval ev

    GetResources _ _                           -> eval_GetResources eval ev
    GetResourceId _ _                          -> eval_GetResourceId eval ev

    GetResourcesLeurons _ _                    -> eval_GetResources eval ev
    GetResourceLeuronLinear _ _ _              -> eval_GetResourceLeuronLinear eval ev
    GetResourceLeuronRandom _ _                -> eval_GetResourceLeuronRandom eval ev
    GetResourcesSiftLeurons _ _                -> eval_GetResourcesSiftLeurons eval ev

    GetLeurons _ _ _                           -> eval_GetLeurons eval ev
    GetLeuronId _ _                            -> eval_GetLeuronId eval ev
    GetLeuronRandom _                          -> eval_GetLeuronRandom eval ev

    GetBuckets _                               -> eval_GetBuckets eval ev
    GetBucketId _ _                            -> eval_GetBucketId eval ev

    GetBucketRounds _ _                        -> eval_GetBucketRounds eval ev
    GetBucketRoundId _ _                       -> eval_GetBucketRoundId eval ev

    ConnectSocket _                            -> eval_ConnectSocket eval ev

  -- Components

    CompArrayString _ _                        -> eval_ArrayString eval ev
    CompProfile _ _                            -> eval_Profile eval ev
    CompResource _ _                           -> eval_Resource eval ev
    CompLeuron _ _                             -> eval_Leuron eval ev
    CompBucket _ _                             -> eval_Bucket eval ev
    CompBucketRound _ _                        -> eval_BucketRound eval ev
    CompOrderBy _ _                            -> eval_OrderBy eval ev
    Nop _                                      -> eval_Nop eval ev
