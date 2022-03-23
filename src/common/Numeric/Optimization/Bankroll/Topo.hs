{-# LANGUAGE FlexibleInstances #-}

module Numeric.Optimization.Bankroll.Topo (
    Measurement(..),
    Reorder(..),
) where

class Measurement a where
    (|>=|) :: a -> a -> Bool
    (|>| ) :: a -> a -> Bool
    lub :: a -> a -> a
    pad :: a -> a
    volume :: a -> Int

instance Measurement () where
    () |>=| () = True
    () |>|  () = False
    lub () () = ()
    pad () = ()
    volume () = 0

instance Measurement Int where
    (|>=|) = (>=)
    (|>| ) = (>)
    lub = max
    pad = (+ 1)
    volume = id

instance Measurement a => Measurement (a,a) where
    (a,b) |>=| (a',b') = a |>=| a' && b |>=| b'
    (a,b) |>|  (a',b') = a |>|  a' && b |>|  b'
    lub (a,b) (a',b') = (lub a a', lub b b')
    pad (a,b) = (pad a, pad b)
    volume (a,b) = volume a * volume b

instance Measurement a => Measurement (a,a,a) where
    (a,b,c) |>=| (a',b',c') = a |>=| a' && (b,c) |>=| (b',c')
    (a,b,c) |>|  (a',b',c') = a |>|  a' && (b,c) |>|  (b',c')
    lub (a,b,c) (a',b',c') = (lub a a', lub b b', lub c c')
    pad (a,b,c) = (pad a, pad b, pad c)
    volume (a,b,c) = volume a * volume (b,c)

instance Measurement a => Measurement (a,a,a,a) where
    (a,b,c,d) |>=| (a',b',c',d') = a |>=| a' && (b,c,d) |>=| (b',c',d')
    (a,b,c,d) |>|  (a',b',c',d') = a |>|  a' && (b,c,d) |>|  (b',c',d')
    lub (a,b,c,d) (a',b',c',d') = (lub a a', lub b b', lub c c', lub d d')
    pad (a,b,c,d) = (pad a, pad b, pad c, pad d)
    volume (a,b,c,d) = volume a * volume (b,c,d)

instance Measurement a => Measurement (a,a,a,a,a) where
    (a,b,c,d,e) |>=| (a',b',c',d',e') = a |>=| a' && (b,c,d,e) |>=| (b',c',d',e')
    (a,b,c,d,e) |>|  (a',b',c',d',e') = a |>|  a' && (b,c,d,e) |>|  (b',c',d',e')
    lub (a,b,c,d,e) (a',b',c',d',e') = (lub a a', lub b b', lub c c', lub d d', lub e e')
    pad (a,b,c,d,e) = (pad a, pad b, pad c, pad d, pad e)
    volume (a,b,c,d,e) = volume a * volume (b,c,d,e)

class Reorder a where
    orderUsing :: a -> a -> a 

instance Reorder () where
    orderUsing = const

instance Reorder Int where
    orderUsing = const

instance Ord a => Reorder (a,a) where
    orderUsing (a,a') (b,b') =
        if b > b'
        then (a',a)
        else (a,a')

instance Ord a => Reorder (a,a,a) where
    orderUsing (a0,a1,a2) (b0,b1,b2) =
        let (c0,c1) = orderUsing (a0,a1) (b0,b1)
            (d0,d1) = orderUsing (b0,b1) (b0,b1)
        in case () of
           _ | d0 > b2   -> (a2,c0,c1)
             | d1 > b2   -> (c0,a2,c1)
             | otherwise -> (c0,c1,a2)

instance Ord a => Reorder (a,a,a,a) where
    orderUsing (a0,a1,a2,a3) (b0,b1,b2,b3) =
        let (c0,c1,c2) = orderUsing (a0,a1,a2) (b0,b1,b2)
            (d0,d1,d2) = orderUsing (b0,b1,b2) (b0,b1,b2)
        in case () of
           _ | d0 > b3   -> (a3,c0,c1,c2)
             | d1 > b3   -> (c0,a3,c1,c2)
             | d2 > b3   -> (c0,c1,a3,c2)
             | otherwise -> (c0,c1,c2,a3)

instance Ord a => Reorder (a,a,a,a,a) where
    orderUsing (a0,a1,a2,a3,a4) (b0,b1,b2,b3,b4) =
        let (c0,c1,c2,c3) = orderUsing (a0,a1,a2,a3) (b0,b1,b2,b3)
            (d0,d1,d2,d3) = orderUsing (b0,b1,b2,b3) (b0,b1,b2,b3)
        in case () of
           _ | d0 > b4   -> (a3,c0,c1,c2,c3)
             | d1 > b4   -> (c0,a3,c1,c2,c3)
             | d2 > b4   -> (c0,c1,a3,c2,c3)
             | d3 > b4   -> (c0,c1,c2,a3,c3)
             | otherwise -> (c0,c1,c2,c3,a3)
