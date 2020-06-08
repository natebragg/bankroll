{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    dense,
    sparse,
    fill,
    components,
    coefficients,
    coefficientOffsets,
    transpose,
) where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Numeric.Algebra (
    Natural,
    Additive(..),
    Abelian,
    Multiplicative(..),
    Semiring,
    Group(..),
    Monoidal(..),
    LeftModule(..),
    RightModule(..),
    sum)
import Numeric.Algebra.Double
import Prelude hiding (sum, negate, (+), (*))

type LinearFunction = LinFunc Int Double

-- For efficiency, it's a critical invariant that LinFuncs are normalized.
-- Never construct with LinFunc, only sparse and dense.
data LinFunc i a where
    LinFunc :: (Ord i, Enum i, Eq a, Monoidal a) => [(i, a)] -> LinFunc i a

deriving instance (Show i, Show a) => Show (LinFunc i a)
deriving instance (Ord i, Ord a) => Ord (LinFunc i a)
deriving instance (Eq i, Eq a) => Eq (LinFunc i a)

instance Foldable (LinFunc i) where
    foldMap f (LinFunc cs) = go (toEnum 0) cs
        where go _ [] = mempty
              go i cs = f (sum $ map snd eqc) `mappend` go (succ i) gtc
                    where (eqc, gtc) = span ((i ==) . fst) cs

    null (LinFunc cs) = null cs

    length (LinFunc []) = 0
    length (LinFunc cs) = 1 + fromEnum (maximum (map fst cs))

    elem a (LinFunc cs) = any ((a ==) . snd) cs

instance Additive a => Additive (LinFunc i a) where
    (LinFunc f) + (LinFunc f') = sparse $ merge f f'
        where merge  f [] = f
              merge [] f' = f'
              merge f@(ia@(i, a):ias) f'@(ia'@(i', a'):ia's) =
                case compare i i' of
                    LT -> ia:merge ias f'
                    EQ -> (i, a + a'):merge ias ia's
                    GT -> ia':merge f ia's

instance RightModule r' r => RightModule r' (LinFunc i r) where
    (LinFunc f) *. n = sparse $ fmap (fmap (*. n)) f

instance LeftModule r' r => LeftModule r' (LinFunc i r) where
    n .* (LinFunc f) = sparse $ fmap (fmap (n .*)) f

instance (Ord i, Enum i, Eq a, Monoidal a, Additive a) => Monoidal (LinFunc i a) where
    zero = LinFunc []

instance (Ord i, Enum i, Eq a, Monoidal a, Group a) => Group (LinFunc i a) where
    negate (LinFunc f) = LinFunc $ fmap (fmap negate) f

groupindex :: Ord i => [(i, a)] -> [(i, [a])]
groupindex = map ((\(a:_, bs) -> (a, bs)) . unzip) .
             groupBy ((==) `on` fst) . sortOn fst

dense :: (Ord i, Enum i, Eq a, Monoidal a) => [a] -> LinFunc i a
dense = sparse . zip (enumFrom $ toEnum 0)

sparse :: (Ord i, Enum i, Eq a, Monoidal a) => [(i, a)] -> LinFunc i a
sparse = LinFunc . normalize
    where normalize = filter ((/= zero) . snd) . map (fmap sum) . groupindex

fill :: (Ord i, Enum i, Eq a, Monoidal a) => [i] -> a -> LinFunc i a
fill is = sparse . zip is . repeat

components :: (Ord i, Enum i, Eq a, Monoidal a) => LinFunc i a -> [LinFunc i a]
components (LinFunc cs) = map (LinFunc . pure) cs

coefficients :: LinFunc i a -> ([i], [a])
coefficients (LinFunc cs) = unzip cs

coefficientOffsets :: [LinFunc i a] -> ([Int], [i], [a])
coefficientOffsets fs = (offs, concat is, concat as)
    where offs = scanl (+) 0 $ map length is
          (is, as) = unzip $ map coefficients fs

transpose :: (Ord i, Enum i, Eq a, Monoidal a) => [LinFunc i a] -> [LinFunc i a]
transpose fs = normalize $ groupindex $ concat $ zipWith reindex (enumFrom $ toEnum 0) fs
    where reindex j (LinFunc cs) = map (fmap $ (,) j) cs
          normalize = go (toEnum 0)
            where go _ [] = []
                  go j fs@((i, _):_ ) | j < i = zero    :go (succ j) fs
                  go j    ((_, f):fs)         = sparse f:go (succ j) fs
