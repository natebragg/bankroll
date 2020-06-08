{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck (infiniteListOf, vectorOf, shuffle, resize, generate, choose, elements, Gen(..))
import Test.QuickCheck.Modifiers (NonZero(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

--import Randoms (problems)
import Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    dense,
    sparse,
    coefficients,
    )
import Numeric.Optimization.Bankroll.Program (
    LinearProgram(..),
    StandardConstraint(..),
    StandardForm(..),
    )
import Numeric.Optimization.Bankroll.Pretty (
    prettyShow,
    )
import Numeric.Optimization.Bankroll.Clp (
    SimplexSolver,
    doClpSolver,
    )

notBothZero :: Gen [Double]
notBothZero = do
    a <- arbitrary
    NonZero b <- arbitrary
    shuffle [a, b]

instance Arbitrary StandardForm where
    arbitrary = StandardForm <$> (dense <$> notBothZero) <*> vectorOf 15 (Lteq <$> (dense <$> notBothZero) <*> arbitrary)

class Perturb a where
    perturb :: a -> Gen a

instance Perturb LinearFunction where
    perturb f = sparse <$> perturb (uncurry zip $ coefficients f)

instance Perturb StandardConstraint where
    perturb (Lteq f d) = Lteq <$> perturb f <*> perturb d

instance Perturb StandardForm where
    perturb (StandardForm os cs) = do
        os' <- perturb os
        cs' <- perturb $ take 9000 $ concat $ repeat cs
        return $ StandardForm os' cs'

instance Perturb Int where
    perturb n = do
        bias <- choose (-1, 1)
        return $ max 0 $ bias + n

instance Perturb Double where
    perturb n = do
        scale <- choose (0.95, 1.05)
        return $ scale * n

instance (Perturb a, Perturb b) => Perturb (a, b) where
    perturb (a, b) = do
        a' <- perturb a
        b' <- perturb b
        return (a', b')

instance Perturb a => Perturb [a] where
    perturb [] = return []
    perturb (x:xs) = do
        x' <- perturb x
        xs' <- perturb xs
        return $ x':xs'

make_problems :: IO ()
make_problems = do
    feasible <- generate $ take 10 <$> filter (not . null . fst . doClpSolver . solve) <$> infiniteListOf (resize 1000 $ arbitrary :: Gen StandardForm)
    problems <- generate $ take 10 <$> filter (not . null . fst . doClpSolver . solve) <$> infiniteListOf (resize 5 $ elements feasible >>= perturb)
    putStrLn "module Randoms (problems) where"
    putStrLn "import Numeric.Optimization.Bankroll.Program (StandardForm)"
    putStr   "problems = "
    putStr $ prettyShow problems

main :: IO ()
main = make_problems
--main = print $ map solve problems
