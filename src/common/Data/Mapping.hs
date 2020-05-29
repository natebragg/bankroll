{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Mapping (
    Mapping(..),
    partitionBy,
    partition,
    mapAppend,
    mapConcat,
    mapUnion,
    mapInvert,
    zipKeyDef,
    (<?<),
    lookupWithDefault,
    substitute,
    updateAll,
    selectAll,
    deleteAll,
    partitionAll,
    member,
) where

import Control.Arrow ((***), (&&&))
import Control.Monad (join)
import Data.Maybe (listToMaybe, isJust)
import Data.Semigroup (Semigroup(..))
import Prelude hiding (lookup)

class Mapping m k v | m -> k v where
    lookupBy :: (k -> Bool) -> m -> Maybe v
    lookupBy = ((listToMaybe . values) .) . selectBy
    updateBy :: (k -> Bool) -> Maybe k -> v -> m -> m
    deleteBy :: (k -> Bool) -> m -> m
    deleteBy = selectBy . (not .)
    selectBy :: (k -> Bool) -> m -> m
    selectBy = deleteBy . (not .)

    fromList :: [(k, v)] -> m
    elements :: m -> [(k, v)]
    keys :: m -> [k]
    keys = map fst . elements
    values :: m -> [v]
    values = map snd . elements

    (<<<) :: (Eq v, Mapping m2 v v2, Mapping m3 k v2) => m2 -> m -> m3
    m2 <<< m = fromList [(k, v2) | (k, v) <- elements m, Just v2 <- [lookup v m2]]

    lookup :: Eq k => k -> m -> Maybe v
    lookup = lookupBy . (==)
    update :: Eq k => k -> v -> m -> m
    update k = updateBy (k ==) (Just k)
    delete :: Eq k => k -> m -> m
    delete = deleteBy . (==)
    select :: Eq k => k -> m -> m
    select = selectBy . (==)

partitionBy :: Mapping m k v => (k -> Bool) -> m -> (m, m)
partitionBy = uncurry (&&&) . (selectBy &&& deleteBy)

partition :: (Eq k, Mapping m k v) => k -> m -> (m, m)
partition = uncurry (&&&) . (select &&& delete)

mapCombine :: (Eq k, Mapping m k v) => (v -> v -> v) -> m -> m -> m
mapCombine f as bs = foldl combine as $ elements bs
    where combine as (k, v) = update k (maybe v (flip f v) (lookup k as)) as

mapAppend :: (Semigroup v, Eq k, Mapping m k v) => m -> m -> m
mapAppend = mapCombine (<>)

mapConcat :: (Semigroup v, Eq k, Mapping m k v) => [m] -> m
mapConcat = foldr mapAppend $ fromList []

mapUnion :: (Eq k, Mapping m k v) => [m] -> m
mapUnion = foldr updateAll $ fromList []

mapInvert :: (Eq v, Mapping m k v, Mapping m' v [k]) => m -> m'
mapInvert m = mapConcat $ [fromList [(v, [k])] | (k, v) <- elements m]

zipKeyDef :: (Eq k, Mapping m1 k v1, Mapping m2 k v2, Mapping m3 k (v1, v2)) => v1 -> v2 -> m1 -> m2 -> m3
zipKeyDef d1 d2 m1 m2 = fromList $ map (fmap (head . (++ [d1]) *** head . (++ [d2]))) $
                                       mapAppend (map (fmap (pure &&& const mempty)) $ elements m1)
                                                 (map (fmap (const mempty &&& pure)) $ elements m2)

(<?<) :: (Eq k, Mapping m k v, Mapping m2 k k) => m -> m2 -> m
m <?< m2 = fromList [(maybe k id $ lookup k m2, v) | (k, v) <- elements m]

lookupWithDefault :: (Eq k, Mapping m k v) => k -> v -> m -> v
lookupWithDefault k v = maybe v id . lookup k

substitute :: (Eq k, Functor f, Mapping m k k) => m -> f k -> f k
substitute = fmap . flip (join lookupWithDefault)

updateAll :: (Eq k, Mapping m k v) => m -> m -> m
updateAll = mapCombine const

selectAll :: (Eq k, Mapping m k v) => [k] -> m -> m
selectAll = selectBy . flip elem

deleteAll :: (Eq k, Mapping m k v) => [k] -> m -> m
deleteAll = flip $ foldl $ flip delete

partitionAll :: (Eq k, Mapping m k v) => [k] -> m -> (m, m)
partitionAll = uncurry (&&&) . (selectAll &&& deleteAll)

member :: (Eq k, Mapping m k v) => k -> m -> Bool
member = (isJust .) . lookup

instance Mapping [(k, v)] k v where
    lookupBy f = go
        where go [] = Nothing
              go ((k,v):_) | f k = Just v
              go (_:kvs) = go kvs
    updateBy f k v = go k
        where go k [] = maybe [] (\k -> [(k, v)]) k
              go _ ((k,_):kvs) | f k = (k,v):go Nothing kvs
              go k (kv:kvs) = kv:go k kvs
    deleteBy f = go
        where go [] = []
              go ((k,_):kvs) | f k = go kvs
              go (kv:kvs) = kv:go kvs

    fromList = id
    elements = id
