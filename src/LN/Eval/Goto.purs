module LN.Eval.Goto (
  eval_Goto
) where



import Control.Monad.Aff.Free   (fromAff)
import Data.Ebyam               (ebyam)
import Data.Functor             (($>))
import Data.Map                 as M
import Data.Maybe               (Maybe(..), maybe)
import Halogen                  (get, gets, modify, liftAff)
import Optic.Core               ((^.),(..))
import Prelude                  (show, bind, pure, unit, id, (==), (/=), (<), ($), (<$>))

import Purescript.Api.Helpers   (qp)

import LN.Component.Types       (EvalEff)
import LN.Input.Types
import LN.Internal.Leuron       (defaultLeuronRequest, leuronToTyLeuron)
import LN.Internal.Resource     (defaultResourceRequest, resourceTypeToTyResourceType)
import LN.Internal.Bucket       (defaultBucketRequest)
import LN.Internal.BucketRound  (defaultBucketRoundRequest)
import LN.Router.Link           (updateUrl)
import LN.Router.Types          (Routes(..), CRUD(..))
import LN.Router.Class.Params   (lookupParam)
import LN.State.PageInfo        ( defaultPageInfo_Resources
                                , defaultPageInfo_Leurons
                                , defaultPageInfo_Users
                                , defaultPageInfo_Buckets
                                , defaultPageInfo_BucketRounds)
import LN.State.Leuron          (defaultLeuronRequestState, leuronRequestStateFromLeuronData)
import LN.State.Resource        (defaultResourceRequestState)
import LN.State.Bucket          (defaultBucketRequestState)
import LN.State.BucketRound     (defaultBucketRoundRequestState)
import LN.T
import LN.T.Convert




-- eval_Goto :: Partial => EvalEff
-- eval_Goto eval (Goto route next) = do
-- eval_Goto :: forall eff. String -> Input -> NaturalTransformation Input (ComponentDSL State Input Void (LNEff eff))
-- eval_Goto eval route nxxt = do

eval_Goto :: Partial => EvalEff
eval_Goto eval (Goto route next) = do

  modify (_ { currentPage = route })
  liftAff $ updateUrl route

  st <- get

  case route of



    Me          -> eval (GetMe next) $> unit



    Errors      -> pure unit



    -- Resources
    --
    (Resources Index params) -> do

      pageInfo <- gets _.resourcesPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Resources.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ resourcesPageInfo = pageInfo { currentPage = offset } })

      eval (GetResources [] next) $> unit



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

      eval (GetLeurons (Just resource_id) [] next) $> unit

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




    -- Leurons
    --
    (Leurons Index params) -> do

      pageInfo <- gets _.leuronsPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Leurons.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ leuronsPageInfo = pageInfo { currentPage = offset } })

      eval (GetLeurons Nothing [] next) $> unit



    (Leurons New params) -> do
      modify (_{ currentLeuronRequest = Just defaultLeuronRequest, currentLeuronRequestSt = Just defaultLeuronRequestState })
      pure unit

    (Leurons (EditI leuron_id) params)   -> do
      eval (GetLeuronId leuron_id next)
      m_pack <- gets _.currentLeuron
      case m_pack of
           Nothing                          -> pure unit
           Just (LeuronPackResponse pack) -> do
             -- TODO FIXME: St's TyLeuronType needs to match source
             let
               leuron = pack.leuron ^. _LeuronResponse
               rst      = defaultLeuronRequestState
             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron, currentLeuronRequestSt = Just rst })
             pure unit

    (Leurons (DeleteI leuron_id) params) -> do
      eval (GetLeuronId leuron_id next)
      m_pack <- gets _.currentLeuron
      case m_pack of
           Nothing                          -> pure unit
           Just (LeuronPackResponse pack) -> do
             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron })
             pure unit


    (Leurons (ShowI leuron_id) params)   -> eval (GetLeuronId leuron_id next) $> unit




    -- Buckets
    --
    (Buckets Index params) -> do

      pageInfo <- gets _.bucketsPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Buckets.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ bucketsPageInfo = pageInfo { currentPage = offset } })

      eval (GetBuckets next) $> unit

    (Buckets New params) -> do
      modify (_{ currentBucketRequest = Just defaultBucketRequest, currentBucketRequestSt = Just defaultBucketRequestState })
      pure unit

    (Buckets (EditI bucket_id) params)   -> do
      eval (GetBucketId bucket_id next)
      m_pack <- gets _.currentBucket
      case m_pack of
           Nothing                          -> pure unit
           Just (BucketPackResponse pack) -> do
             let
               rst      = defaultBucketRequestState
             modify (_{ currentBucketRequest = Just $ bucketResponseToBucketRequest pack.bucket, currentBucketRequestSt = Just rst })
             pure unit

    (Buckets (DeleteI bucket_id) params) -> do
      eval (GetBucketId bucket_id next)
      m_pack <- gets _.currentBucket
      case m_pack of
           Nothing                          -> pure unit
           Just (BucketPackResponse pack) -> do
             modify (_{ currentBucketRequest = Just $ bucketResponseToBucketRequest pack.bucket })
             pure unit

    (Buckets (ShowI bucket_id) params)   -> eval (GetBucketId bucket_id next) $> unit



    (BucketsResources bucket_id _ params) -> do

      eval (GetBucketId bucket_id next) $> unit

      pageInfo <- gets _.resourcesPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Resources.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ resourcesPageInfo = pageInfo { currentPage = offset } })

      -- if "myStuff" is checked, only retrieve resources added to this bucket
      mine <- maybe false _.myStuff <$> gets _.currentBucketRequestSt
      let params = if mine then [ByBucketId bucket_id] else []
      eval (GetResources params next) $> unit



    (BucketsLeurons bucket_id _ params) -> do

      eval (GetBucketId bucket_id next) $> unit

      pageInfo <- gets _.leuronsPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Leurons.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ leuronsPageInfo = pageInfo { currentPage = offset } })

      -- if "myStuff" is checked, only retrieve leurons added to this bucket
      mine <- maybe false _.myStuff <$> gets _.currentBucketRequestSt
      let params = if mine then [ByBucketId bucket_id] else []
      eval (GetLeurons Nothing params next) $> unit




    (BucketsRounds bucket_id Index params) -> do

      eval (GetBucketId bucket_id next) $> unit

      pageInfo <- gets _.bucketRoundsPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_BucketRounds.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ bucketRoundsPageInfo = pageInfo { currentPage = offset } })

      eval (GetBucketRounds bucket_id next) $> unit

    (BucketsRounds bucket_id New params) -> do

      eval (GetBucketId bucket_id next) $> unit

      modify (_{ currentBucketRoundRequest = Just defaultBucketRoundRequest, currentBucketRoundRequestSt = Just defaultBucketRoundRequestState })
      pure unit



    -- Users
    --

    (Users Index params) -> do
      pageInfo <- gets _.usersPageInfo
      let offset = ebyam (lookupParam ParamTag_Offset params) defaultPageInfo_Users.currentPage (\(Offset v) -> if v < 0 then pageInfo.totalPages else v)
      modify (_{ usersPageInfo = pageInfo { currentPage = offset } })
      eval (GetUsers next) $> unit

    (Users (Show user_name) params) -> eval (GetUser user_name next) $> unit



    (UsersProfile user_name params) -> eval (GetUser user_name next) $> unit
    (UsersSettings user_name params) -> eval (GetUser user_name next) $> unit
    (UsersResources user_name params) -> eval (GetUser user_name next) $> unit
    (UsersLeurons user_name params) -> eval (GetUser user_name next) $> unit



    _           -> pure unit



  pure next
