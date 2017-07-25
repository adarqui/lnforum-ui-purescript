module LN.Router.Class.Params (
  Params (..),
  PSRoutingParams,
  emptyParams,
  lookupParam,
  fixParams,
  psRoutingParamsToParams,
  paramTagFromString
) where



import Data.Int                    as I
import Data.List                   (catMaybes)
import Data.Maybe                  (Maybe(..), maybe)
import Data.Map                    as M
import Data.StrMap                 as StrM
import Data.Tuple                  (Tuple(..))
import Prelude                     (id, map, show, ($))

import LN.T                        (Param(..), ParamTag(..), OrderBy(..), SortOrderBy(..))



type Params = StrM.StrMap Param



type PSRoutingParams = M.Map String String



emptyParams :: Params
emptyParams = StrM.empty



lookupParam :: ParamTag -> Params -> Maybe Param
lookupParam p_tag params = StrM.lookup (show p_tag) params



fixParams :: Params -> Params
fixParams = id
-- fixParams :: Params -> PSRoutingParams
-- fixParams params = M.fromList $ map (qp <<< snd) $ M.toList params --  M.fromList <<< arrayToList
-- fixParams params = M.fromList $ map (qp <<< snd) $ M.toList params --  M.fromList <<< arrayToList



psRoutingParamsToParams :: PSRoutingParams -> Params
psRoutingParamsToParams ps =
  StrM.fromFoldable $ catMaybes $ map (\(Tuple k v) -> (paramFromKV' k v)) $ M.toUnfoldable ps



paramFromKV :: String -> String -> Maybe Param
paramFromKV k v = Nothing



paramFromKV' :: String -> String -> Maybe (Tuple String Param)
paramFromKV' k v =
  case paramTagFromString k of
    Nothing    -> Nothing
    Just ParamTag_Limit     -> maybe Nothing (\v -> Just $ Tuple k (Limit v)) (I.fromString v)
    Just ParamTag_Offset    -> maybe Nothing (\v -> Just $ Tuple k (Offset v)) (I.fromString v)
    Just ParamTag_Order     -> Just $ Tuple k (Order $ orderFromString v)
    Just ParamTag_SortOrder -> Just $ Tuple k (SortOrder $ sortOrderFromString v)
    Just _                  -> Nothing



paramTagFromString :: String -> Maybe ParamTag
paramTagFromString s =
  case s of
    "limit"                  ->  Just ParamTag_Limit
    "offset"                 ->  Just ParamTag_Offset
    "sort_order"             ->  Just ParamTag_SortOrder
    "order"                  ->  Just ParamTag_Order
    "user_id"                ->  Just ParamTag_ByUserId
    "users_ids"              ->  Just ParamTag_ByUsersIds
--    "user_nick"              ->  Just ParamTag_ByUserNick
--    "users_nicks"            ->  Just ParamTag_ByUsersNicks
    "bucket_id"              ->  Just ParamTag_ByBucketId
    "resource_id"            ->  Just ParamTag_ByResourceId
    "resources_ids"          ->  Just ParamTag_ByResourcesIds
    "resource_name"          ->  Just ParamTag_ByResourceName
    "leuron_id"              ->  Just ParamTag_ByLeuronId
    "leurons_ids"            ->  Just ParamTag_ByLeuronsIds
    "parent_id"              ->  Just ParamTag_ByParentId
    "parents_ids"            ->  Just ParamTag_ByParentsIds
    "parent_name"            ->  Just ParamTag_ByParentName
    "ts"                     ->  Just ParamTag_Timestamp
    "unix_ts"                ->  Just ParamTag_UnixTimestamp
    "created_at_ts"          ->  Just ParamTag_CreatedAtTimestamp
    "created_at_unix_ts"     ->  Just ParamTag_CreatedAtUnixTimestamp
    "real_ip"                ->  Just ParamTag_RealIP
    "ip"                     ->  Just ParamTag_IP
    _                        ->  Nothing



sortOrderFromString :: String -> SortOrderBy
sortOrderFromString s =
  case s of
    "asc"  -> SortOrderBy_Asc
    "dsc"  -> SortOrderBy_Dsc
    "rand" -> SortOrderBy_Rnd
    _      -> SortOrderBy_None



orderFromString :: String -> OrderBy
orderFromString s =
  case s of
    "user_id"     -> OrderBy_UserId
    "created_at"  -> OrderBy_CreatedAt
    "modified_at" -> OrderBy_ModifiedAt
    "modified_by" -> OrderBy_ModifiedBy
    "activity_at" -> OrderBy_ActivityAt
    "id"          -> OrderBy_Id
    _             -> OrderBy_None
