module LN.Helpers.Map (
  mergeMap,
  mergeMapList,
  mergeMapArray,
  idmapFrom
) where



import Data.Array as A
import Data.List  as L
import Data.List  (List)
import Data.Map   as M
import Data.Map   (Map)
import Data.Ord   (class Ord)
import Prelude    (map, ($))



mergeMap :: forall a b. Ord b => Map b a -> Map b a -> Map b a
mergeMap st m =
  M.union st m



mergeMapList :: forall a b. Ord b => Map b a -> List a -> (a -> b) -> Map b a
mergeMapList st m un_accessor =
  M.union st (M.fromFoldable $ L.zip (map un_accessor m) m)



mergeMapArray :: forall a b. Ord b => Map b a -> Array a -> (a -> b) -> Map b a
mergeMapArray st m un_accessor =
  M.union st (M.fromFoldable $ A.zip (map un_accessor m) m)



idmapFrom :: forall a b. Ord b => (a -> b) -> Array a -> Map b a
idmapFrom un packs = M.fromFoldable $ A.zip (map (\pack -> un pack) packs) packs
