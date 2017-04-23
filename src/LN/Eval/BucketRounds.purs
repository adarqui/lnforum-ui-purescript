module LN.Eval.BucketRounds (
  eval_GetBucketRounds,
  eval_GetBucketRoundId,
  eval_BucketRound
) where



import Data.Array                    (head, deleteAt, modifyAt, nub, (:))
import Data.Either                   (Either(..))
import Data.Functor                  (($>))
import Data.Int                      (fromString)
import Data.Map                      as Map
import Data.Maybe                    (Maybe(..), maybe)
import Data.Tuple
import Data.Tuple.Nested
import Halogen                       (gets, modify)
import Optic.Core                    ((^.), (..), (.~))
import Prelude                       (class Eq, id, const, bind, pure, map, Unit, unit, ($), (<>), (<<<), (==), (<$>), (*>), ($>))

import LN.Api
import LN.Helpers.Api                (rd)
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.BucketRound          (InputBucketRound(..), BucketRound_Mod(..))
import LN.Input.Types                (Input(..))
import LN.Router.Types               (Routes(..), CRUD(..))
import LN.Router.Class.Params        (emptyParams)
import LN.State.BucketRound
import LN.State.Loading              (l_currentBucketRound, l_bucketRounds)
import LN.State.Loading.Helpers      (setLoading, clearLoading)
import LN.State.PageInfo             (runPageInfo)
import LN.T                          ( BucketRoundResponses(..), BucketRoundResponse(..)
                                     , BucketRoundResponse(..)
                                     , BucketRoundRequest(..)
                                     , _BucketRoundRequest
                                     , displayName_, description_, scoreLo_, scoreHi_
                                     , Param(..), SortOrderBy(..)
                                     , SimpleIntsResponse (..)
                                     , BucketPackResponse (..))



eval_GetBucketRounds :: Partial => EvalEff
eval_GetBucketRounds eval (GetBucketRounds bucket_id next) = do

  modify (_{ bucketRounds = (Map.empty :: Map.Map Int BucketRoundResponse) })

  page_info <- gets _.bucketRoundsPageInfo

  e_count <- rd $ getBucketRoundsCount [ByBucketId bucket_id]
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetBucketRounds::getBucketRoundsCount'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_{ bucketRoundsPageInfo = new_page_info.pageInfo })
      modify $ setLoading l_bucketRounds

      e_bucket_rounds <- rd $ getBucketRounds $ [ByBucketId bucket_id] <> new_page_info.params

      modify $ clearLoading l_bucketRounds

      case e_bucket_rounds of
           Left err                                 -> eval (AddErrorApi "eval_GetBucketRounds::getBucketRound" err next)
           Right (BucketRoundResponses bucket_rounds) -> do

             let
              bucketRounds_map = idmapFrom (\(BucketRoundResponse p) -> p.id) bucket_rounds.bucketRoundResponses

             modify (_{ bucketRounds = bucketRounds_map })
             pure next



eval_GetBucketRoundId :: Partial => EvalEff
eval_GetBucketRoundId eval (GetBucketRoundId bucket_round_id next) = do

  modify (\st->st{
           currentBucketRound = Nothing
         , currentBucketRoundRequestSt = maybe (Just defaultBucketRoundRequestState) Just st.currentBucketRoundRequestSt
         })

  modify $ setLoading l_currentBucketRound

  e_round <- rd $ getBucketRound' bucket_round_id

  modify $ clearLoading l_currentBucketRound

  case e_round of
    Right round -> do
      modify (_{ currentBucketRound = Just round })
      pure next



eval_BucketRound :: Partial => EvalEff
eval_BucketRound eval (CompBucketRound sub next) = do

  case sub of
    InputBucketRound_Mod q -> do
      case q of

        Create -> do

          m_req <- gets _.currentBucketRoundRequest

          case m_req of
               Nothing  -> eval (AddError "eval_BucketRound(Create)" "BucketRound request doesn't exist" next)
               Just req -> do

                 m_bucket <- gets _.currentBucket
                 case m_bucket of
                      Nothing -> eval (AddError "eval_BucketRound(Create)" "Bucket doesn't exist" next)
                      Just (BucketPackResponse pack) -> do

                        e_bucket <- rd $ postBucketRound [ByBucketId pack.bucketId] req
                        case e_bucket of
                             Left err                      -> eval (AddErrorApi "eval_BucketRound(Create)::postBucketRound'" err next)
                             Right (BucketRoundResponse round) -> eval (Goto (BucketsRounds pack.bucketId (ShowI round.id) emptyParams) next)

        ModSt f -> do
          modSt f
          route <- gets _.currentPage
          eval (Goto route next)

        ModReq f -> do
          modReq f

    InputBucketRound_Nop         -> pure next

  where
--  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
--  append Nothing a    = Just [a]
--  append (Just arr) a = Just $ nub $ arr <> [a]
--  set v req                = Just (v req)
--  mod new                  = modify (\st->st{ currentBucketRoundRequest = maybe Nothing new st.currentBucketRoundRequest }) $> next
  modSt new                = modify (\st->st{ currentBucketRoundRequestSt = maybe Nothing (Just <<< new) st.currentBucketRoundRequestSt }) $> next
  modReq new               = modify (\st->st{ currentBucketRoundRequest = maybe Nothing (Just <<< new) st.currentBucketRoundRequest }) $> next
