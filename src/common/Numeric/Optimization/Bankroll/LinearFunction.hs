{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    dense,
    sparse,
    fill,
    coordinates,
    components,
    coefficients,
    coefficientOffsets,
    transpose,
    hadamard,
    dot,
    norm,
    within,
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
import Prelude hiding (sum, negate, (+), (-), (*))

type LinearFunction = LinFunc Int Double

-- For efficiency, it's a critical invariant that LinFuncs are normalized.
-- Never construct with LinFunc, only sparse and dense.
data LinFunc i a where
    LinFunc :: (Ord i, Monoidal a) => {
        coordinates :: [(i, a)]
    } -> LinFunc i a

deriving instance (Show i, Show a) => Show (LinFunc i a)
deriving instance (Eq i, Eq a) => Eq (LinFunc i a)

instance Enum i => Foldable (LinFunc i) where
    foldMap f (LinFunc cs) = go (toEnum 0) cs
        where go _ [] = mempty
              go i cs = f (sum $ map snd eqc) `mappend` go (succ i) gtc
                    where (eqc, gtc) = span ((i ==) . fst) cs

    null = null . coordinates

    length (LinFunc []) = 0
    length (LinFunc cs) = 1 + fromEnum (maximum (map fst cs))

    elem a = any ((a ==) . snd) . coordinates

instance (Eq a, Additive a) => Additive (LinFunc i a) where
    (+) = mergeWith (+)

mergeWith :: Eq a => (a -> a -> a) -> LinFunc i a -> LinFunc i a -> LinFunc i a
mergeWith g (LinFunc cs) (LinFunc cs') = sparse $ merge cs cs'
        where merge  f [] = f
              merge [] f' = f'
              merge f@(ia@(i, a):ias) f'@(ia'@(i', a'):ia's) =
                case compare i i' of
                    LT -> ia:merge ias f'
                    EQ -> (i, g a a'):merge ias ia's
                    GT -> ia':merge f ia's

instance (Eq a, RightModule a' a) => RightModule a' (LinFunc i a) where
    (LinFunc f) *. n = sparse $ fmap (fmap (*. n)) f

instance (Eq a, LeftModule a' a) => LeftModule a' (LinFunc i a) where
    n .* (LinFunc f) = sparse $ fmap (fmap (n .*)) f

instance (Ord i, Eq a, Monoidal a) => Monoidal (LinFunc i a) where
    zero = LinFunc []

instance (Ord i, Eq a, Group a) => Group (LinFunc i a) where
    negate = sparse . fmap (fmap negate) . coordinates

groupindex :: Ord i => [(i, a)] -> [(i, [a])]
groupindex = map ((\(a:_, bs) -> (a, bs)) . unzip) .
             groupBy ((==) `on` fst) . sortOn fst

dense :: (Ord i, Enum i, Eq a, Monoidal a) => [a] -> LinFunc i a
dense = sparse . zip (enumFrom $ toEnum 0)

-- normalization invariants: indices sorted/unique, no zero components.
sparse :: (Ord i, Eq a, Monoidal a) => [(i, a)] -> LinFunc i a
sparse = LinFunc . normalize
    where normalize = filter ((/= zero) . snd) . map (fmap sum) . groupindex

fill :: (Ord i, Eq a, Monoidal a) => [i] -> a -> LinFunc i a
fill is = sparse . zip is . repeat

components :: (Ord i, Eq a, Monoidal a) => LinFunc i a -> [LinFunc i a]
components = map (LinFunc . pure) . coordinates

coefficients :: LinFunc i a -> ([i], [a])
coefficients = unzip . coordinates

coefficientOffsets :: [LinFunc i a] -> ([Int], [i], [a])
coefficientOffsets fs = (offs, concat is, concat as)
    where offs = scanl (+) 0 $ map length is
          (is, as) = unzip $ map coefficients fs

transpose :: (Ord i, Enum i, Eq a, Monoidal a) => [LinFunc i a] -> [LinFunc i a]
transpose fs = normalize $ groupindex $ concat $ zipWith reindex (enumFrom $ toEnum 0) fs
    where reindex j = map (fmap $ (,) j) . coordinates
          normalize = go (toEnum 0)
            where go _ [] = []
                  go j fs@((i, _):_ ) | j < i = zero    :go (succ j) fs
                  go j    ((_, f):fs)         = sparse f:go (succ j) fs

hadamard :: (Eq a, Multiplicative a) => LinFunc i a -> LinFunc i a -> LinFunc i a
hadamard = mergeWith (*)

dot :: (Eq a, Multiplicative a, Monoidal a) => LinFunc i a -> LinFunc i a -> a
dot = ((sum . map snd . coordinates) .) . hadamard

norm :: (Eq a, Multiplicative a, Monoidal a, Floating a) => LinFunc i a -> a
norm f = sqrt $ dot f f

within :: (Ord i, Ord a, Multiplicative a, Group a, Floating a) => a -> LinFunc i a -> LinFunc i a -> Bool
within epsilon f g = epsilon > norm (f - g)
