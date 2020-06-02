{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    dense,
    sparse,
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
    foldMap f (LinFunc cs) = go (toEnum 0) $ sortOn fst cs
        where go _ [] = mempty
              go i cs = f (sum $ map snd eqc) `mappend` go (succ i) gtc
                    where (eqc, gtc) = span ((i ==) . fst) cs

    null (LinFunc cs) = null cs

    length (LinFunc []) = 0
    length (LinFunc cs) = 1 + fromEnum (maximum (map fst cs))

    elem a (LinFunc cs) = any ((a ==) . snd) cs

instance Additive a => Additive (LinFunc i a) where
    (LinFunc f) + (LinFunc f') = sparse $ merge (sortOn fst f) (sortOn fst f')
        where merge  f [] = f
              merge [] f' = f'
              merge f@(ia@(i, a):ias) f'@(ia'@(i', a'):ia's) =
                case compare i i' of
                    LT -> ia:merge ias f'
                    EQ -> (i, a + a'):merge ias ia's
                    GT -> ia':merge f ia's

instance Semiring r => RightModule r (LinFunc i r) where
    (LinFunc f) *. n = sparse $ fmap (fmap (* n)) f

instance Semiring r => LeftModule r (LinFunc i r) where
    (.*) = flip (*.)

instance Additive r => RightModule Natural (LinFunc i r) where
    m *. n = m *. fromIntegral n

instance Additive r => LeftModule Natural (LinFunc i r) where
    (.*) = (.*) . fromIntegral

instance (Ord i, Enum i, Eq a, Monoidal a, Additive a) => Monoidal (LinFunc i a) where
    zero = LinFunc []

instance Additive r => RightModule Integer (LinFunc i r) where
    m *. n = m *. fromIntegral n

instance Additive r => LeftModule Integer (LinFunc i r) where
    (.*) = (.*) . fromIntegral

instance (Ord i, Enum i, Eq a, Monoidal a, Group a) => Group (LinFunc i a) where
    negate (LinFunc f) = LinFunc $ fmap (fmap negate) f

dense :: (Ord i, Enum i, Eq a, Monoidal a) => [a] -> LinFunc i a
dense = sparse . zip (enumFrom $ toEnum 0)

sparse :: (Ord i, Enum i, Eq a, Monoidal a) => [(i, a)] -> LinFunc i a
sparse = LinFunc . filter ((/= zero) . snd)

coefficients :: LinFunc i a -> ([i], [a])
coefficients (LinFunc cs) = unzip cs

coefficientOffsets :: [LinFunc i a] -> ([Int], [i], [a])
coefficientOffsets fs = (offs, concat is, concat as)
    where offs = scanl (+) 0 $ map length is
          (is, as) = unzip $ map coefficients fs

transpose :: (Ord i, Enum i, Eq a, Monoidal a) => [LinFunc i a] -> [LinFunc i a]
transpose fs = normalize $ groupindex $ concat $ zipWith reindex (enumFrom $ toEnum 0) fs
    where reindex j (LinFunc cs) = map (fmap $ (,) j) cs
          groupindex = map ((\(a:_, bs) -> (a, bs)) . unzip) .
                       groupBy ((==) `on` fst) . sortOn fst
          normalize = go (toEnum 0)
            where go _ [] = []
                  go j fs@((i, _):_ ) | j < i = zero    :go (succ j) fs
                  go j    ((_, f):fs)         = sparse f:go (succ j) fs
