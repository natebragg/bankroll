{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    dense,
    sparse,
    fill,
    coordinates,
    components,
    coefficients,
    coefficientOffsets,
    project,
    transpose,
    hadamard,
    dot,
    norm,
    manhattan,
    within,
) where

import Control.Arrow (first)
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
import Numeric.Optimization.Bankroll.Topo (
    Measurement(..),
    Reorder(..),
    )
import Prelude hiding (sum, negate, (+), (-), (*))

type LinearFunction = LinFunc Int Double

-- For efficiency, it's a critical invariant that LinFuncs are normalized.
-- Never construct with LinFunc, only sparse and dense.
data LinFunc i a where
    LinFunc :: (Ord i, Monoidal a) => {
        coordinates :: [(i, a)],
        shape :: i
    } -> LinFunc i a

deriving instance (Show i, Show a) => Show (LinFunc i a)
deriving instance (Eq i, Eq a) => Eq (LinFunc i a)

instance Enum i => Foldable (LinFunc i) where
    foldMap f (LinFunc cs s) = go (enumFromTo (toEnum 0) s) cs
        where go [] [] = mempty
              go []  _ = error "Bug in foldMap"
              go (i:is) [] = f zero `mappend` go is []
              go (i:is) cs = f (sum $ map snd eqc) `mappend` go is gtc
                    where (eqc, gtc) = span ((i ==) . fst) cs

    null = null . coordinates

    length (LinFunc _ s) = fromEnum s

    elem a = any ((a ==) . snd) . coordinates

instance (Enum i, Measurement i, Eq a, Additive a) => Additive (LinFunc i a) where
    (+) = mergeWith (+)

mergeWith :: (Enum i, Measurement i, Eq a) => (a -> a -> a) -> LinFunc i a -> LinFunc i a -> LinFunc i a
mergeWith g (LinFunc cs s) (LinFunc cs' s') = resize (lub s s') $ sparse $ merge cs cs'
        where merge  f [] = map (fmap $ flip g zero) f
              merge [] f' = map (fmap $      g zero) f'
              merge f@((i, a):ias) f'@((i', a'):ia's) =
                case compare i i' of
                    LT -> (i,  g a zero ):merge ias f'
                    EQ -> (i,  g a    a'):merge ias ia's
                    GT -> (i', g zero a'):merge f ia's

instance (Enum i, Measurement i, Eq a, RightModule a' a) => RightModule a' (LinFunc i a) where
    (LinFunc f s) *. n = resize s $ sparse $ fmap (fmap (*. n)) f

instance (Enum i, Measurement i, Eq a, LeftModule a' a) => LeftModule a' (LinFunc i a) where
    n .* (LinFunc f s) = resize s $ sparse $ fmap (fmap (n .*)) f

instance (Ord i, Enum i, Measurement i, Eq a, Monoidal a) => Monoidal (LinFunc i a) where
    zero = LinFunc [] $ toEnum 0

instance (Ord i, Enum i, Measurement i, Eq a, Group a) => Group (LinFunc i a) where
    negate (LinFunc cs s) = resize s $ sparse $ fmap (fmap negate) cs

groupindex :: Ord i => [(i, a)] -> [(i, [a])]
groupindex = map ((\(a:_, bs) -> (a, bs)) . unzip) .
             groupBy ((==) `on` fst) . sortOn fst

dense :: (Ord i, Enum i, Measurement i, Eq a, Monoidal a) => [a] -> LinFunc i a
dense = sparse . zip (enumFrom $ toEnum 0)

-- normalization invariants: indices sorted/unique, no zero components.
sparse :: (Ord i, Enum i, Measurement i, Eq a, Monoidal a) => [(i, a)] -> LinFunc i a
sparse ias = LinFunc (normalize ias) $ pad $ foldr lub (toEnum 0) $ map fst ias
    where normalize = filter ((/= zero) . snd) . map (fmap sum) . groupindex

fill :: (Ord i, Enum i, Measurement i, Eq a, Monoidal a) => [i] -> a -> LinFunc i a
fill is = sparse . zip is . repeat

resize :: Measurement i => i -> LinFunc i a -> LinFunc i a
resize s (LinFunc cs s_old) =
    if volume s >= volume s_old
    then LinFunc cs s
    else error "Expected new size to exceed old size"

components :: (Ord i, Eq a, Monoidal a) => LinFunc i a -> [LinFunc i a]
components (LinFunc cs s) = map (flip LinFunc s . pure) cs

coefficients :: LinFunc i a -> ([i], [a])
coefficients = unzip . coordinates

coefficientOffsets :: [LinFunc i a] -> ([Int], [i], [a])
coefficientOffsets fs = (offs, concat is, concat as)
    where offs = scanl (+) 0 $ map length is
          (is, as) = unzip $ map coefficients fs

project :: (Measurement i, Group i) => i -> i -> LinFunc i a -> LinFunc i a
project from to (LinFunc cs s) = LinFunc (map offset $ filter within cs) (to - from)
    where within (i, _) = i |>=| from && to |>| i
          offset (i, a) = (i - from, a)

traspose1 :: (Enum i, Measurement i, Reorder i, Eq a) => i -> LinFunc i a -> LinFunc i a
traspose1 order (LinFunc cs s) =
    resize (s `orderUsing` order) $
        sparse (map (first (`orderUsing` order)) cs)

transpose :: (Ord i, Enum i, Measurement i, Eq a, Monoidal a) => [LinFunc i a] -> [LinFunc i a]
transpose fs = normalize $ groupindex $ concat $ zipWith reindex (enumFrom $ toEnum 0) fs
    where reindex j = map (fmap $ (,) j) . coordinates
          fl = toEnum $ length fs
          fw = foldr lub (toEnum 0) $ map shape fs
          normalize = go $ enumFromTo (toEnum 0) $ pred fw
            where go [] [] = []
                  go []  _ = error "Bug in transpose"
                  go (j:js) []                     = resize fl zero      :go js []
                  go (j:js) fs@((i, _):_ ) | j < i = resize fl zero      :go js fs
                  go (j:js)    ((_, f):fs)         = resize fl (sparse f):go js fs

hadamard :: (Enum i, Measurement i, Eq a, Multiplicative a) => LinFunc i a -> LinFunc i a -> LinFunc i a
hadamard = mergeWith (*)

dot :: (Enum i, Measurement i, Eq a, Multiplicative a, Monoidal a) => LinFunc i a -> LinFunc i a -> a
dot = ((sum . map snd . coordinates) .) . hadamard

norm :: (Enum i, Measurement i, Eq a, Multiplicative a, Monoidal a, Floating a) => LinFunc i a -> a
norm f = sqrt $ dot f f

manhattan :: (Enum i, Monoidal a, Num a) => LinFunc i a -> a
manhattan = sum . map abs . snd . coefficients

within :: (Ord i, Enum i, Measurement i, Ord a, Multiplicative a, Group a, Floating a) => a -> LinFunc i a -> LinFunc i a -> Bool
within epsilon f g = epsilon > norm (f - g)
