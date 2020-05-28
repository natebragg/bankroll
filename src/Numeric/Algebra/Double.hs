{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeric.Algebra.Double where

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
    )
import Prelude hiding (sum, negate, (+), (*))
import qualified Prelude as P (negate, (+), (*))

-- These instances are not lawful; they are inexact.
instance Additive Double where
    (+) = (P.+)

instance Abelian Double

instance Multiplicative Double where
    (*) = (P.*)

instance Semiring Double

instance (Semiring r, Real r) => RightModule r Double where
    m *. n = m * realToFrac n

instance (Semiring r, Real r) => LeftModule r Double where
    (.*) = (*) . realToFrac

instance RightModule Double Integer where
    m *. n = round $ fromIntegral m * n

instance LeftModule Double Integer where
    m .* n = round $ m * fromIntegral n

instance Monoidal Double where
    zero = 0.0

instance Group Double where
    negate = P.negate
