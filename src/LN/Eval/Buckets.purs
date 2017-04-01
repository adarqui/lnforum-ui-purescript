module LN.Eval.Buckets (
  eval_GetBuckets,
  eval_GetBucketId,
  eval_Bucket
) where



import Data.Array                    (head, deleteAt, modifyAt, nub, (:))
import Data.Either                   (Either(..))
import Data.Functor                  (($>))
import Data.Int                      (fromString)
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Halogen                       (gets, modify)
import Optic.Core                    ((^.), (..), (.~))
import Prelude                       (class Eq, id, const, bind, pure, map, ($), (<>), (<<<), (==))

import LN.Api
import LN.Helpers.Api                (rd)
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.Bucket               (InputBucket(..), Bucket_Mod(..))
import LN.Input.Types                (Input(..))
import LN.State.Bucket
import LN.State.Loading              (l_currentBucket, l_buckets)
import LN.State.Loading.Helpers      (setLoading, clearLoading)
import LN.State.PageInfo             (runPageInfo)
import LN.T                          ( BucketPackResponses(..), BucketPackResponse(..)
                                     , BucketResponse(..)
                                     , BucketRequest(..)
                                     , _BucketRequest
                                     , displayName_, description_, scoreLo_, scoreHi_
                                     , Param(..), SortOrderBy(..))



eval_GetBuckets :: Partial => EvalEff
eval_GetBuckets eval (GetBuckets next) = do

  modify (_{ buckets = (M.empty :: M.Map Int BucketPackResponse) })

  page_info <- gets _.bucketsPageInfo

  e_count <- rd $ getBucketsCount'
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetBuckets::getBucketsCount'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_{ bucketsPageInfo = new_page_info.pageInfo })
      modify $ setLoading l_buckets

      e_bucket_packs <- rd $ getBucketPacks $ new_page_info.params

      modify $ clearLoading l_buckets

      case e_bucket_packs of
           Left err                                 -> eval (AddErrorApi "eval_GetBuckets::getBucketPacks" err next)
           Right (BucketPackResponses bucket_packs) -> do

             let
              users       = map (\(BucketPackResponse pack) -> pack.user) bucket_packs.bucketPackResponses
              buckets_map = idmapFrom (\(BucketPackResponse p) -> p.bucketId) bucket_packs.bucketPackResponses


             eval (GetUsers_MergeMap_ByUser users next)


             modify (_{ buckets = buckets_map })
             pure next



eval_GetBucketId :: Partial => EvalEff
eval_GetBucketId eval (GetBucketId bucket_id next) = do

  modify (_{ currentBucket = Nothing })
  modify $ setLoading l_currentBucket

  e_pack <- rd $ getBucketPack' bucket_id

  modify $ clearLoading l_currentBucket

  case e_pack of
    Left err   -> eval (AddErrorApi "eval_GetBucketId::getBucketPack'" err next)
    Right pack -> do
      modify (_{ currentBucket = Just pack })
      pure next



eval_Bucket :: Partial => EvalEff
eval_Bucket eval (CompBucket sub next) = do

  case sub of
    InputBucket_Mod q -> do
      case q of

        SetDisplayName display_name      -> mod $ set (\req -> _BucketRequest .. displayName_ .~ display_name $ req)
        EditDisplayName display_name     -> pure next
        RemoveDisplayName                -> pure next

        SetDescription desc  -> mod $ set (\req -> _BucketRequest .. description_ .~ Just desc $ req)
        EditDescription desc -> pure next
        RemoveDescription    -> pure next

        Create -> do

          m_req <- gets _.currentBucketRequest

          case m_req of
               Nothing  -> eval (AddError "eval_Bucket(Create)" "Bucket request doesn't exist" next)
               Just req -> do

                 e_bucket <- rd $ postBucket' req
                 case e_bucket of
                      Left err                      -> eval (AddErrorApi "eval_Bucket(Create)::postBucket'" err next)
                      Right (BucketResponse bucket) -> pure next

        EditP bucket_id    -> do

          m_req <- gets _.currentBucketRequest

          case m_req of
               Nothing  -> eval (AddError "eval_Bucket(Edit)" "Bucket request doesn't exist" next)
               Just req -> do

                 e_bucket <- rd $ putBucket' bucket_id req
                 case e_bucket of
                      Left err                          -> eval (AddErrorApi "eval_Bucket(Edit)::putBucket'" err next)
                      Right (BucketResponse bucket) -> pure next
--                        eval (Goto (Buckets (ShowI bucket.id) []) next)

    InputBucket_Nop         -> pure next

  where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req                = Just (v req)
  mod new                  = modify (\st->st{ currentBucketRequest = maybe Nothing new st.currentBucketRequest }) $> next
  -- modSt new                = modify (\st->st{ currentBucketRequestSt = maybe Nothing (Just <<< new) st.currentBucketRequestSt }) $> next
