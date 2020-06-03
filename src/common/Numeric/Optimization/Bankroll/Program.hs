module Numeric.Optimization.Bankroll.Program (
    LinearProgram(..),
    Solution,
    Objective,
    LinearFunction,
    GeneralConstraint(..), (<=$), (==$), (>=$),
    GeneralForm(..),
    StandardConstraint(..),
    StandardForm(..),
) where

import qualified Numeric.Optimization.Bankroll.Solver as Solver
import Numeric.Optimization.Bankroll.LinearFunction (LinearFunction, dense)

import Control.Arrow ((***))
import Data.Foldable (toList)
import Numeric.Algebra (zero)

inf = read "Infinity"

type Solution = LinearFunction

class LinearProgram a where
    solve :: Solver.MonadSolver m => a -> m (Solution, Double)

type Objective = LinearFunction

data GeneralConstraint = Leq LinearFunction Double
                       | Eql LinearFunction Double
                       | Geq LinearFunction Double
    deriving Show

(<=$) = Leq
(==$) = Eql
(>=$) = Geq

infix 4 <=$
infix 4 ==$
infix 4 >=$

lf :: GeneralConstraint -> LinearFunction
lf (Leq f _) = f
lf (Eql f _) = f
lf (Geq f _) = f

bound :: GeneralConstraint -> (Double, Double)
bound (Leq _ n) = (-inf, n)
bound (Eql _ n) = (n, n)
bound (Geq _ n) = (n, inf)

data GeneralForm = GeneralForm Solver.OptimizationDirection Objective [GeneralConstraint]
    deriving Show

instance LinearProgram GeneralForm where
    solve (GeneralForm direction objective constraints) = do
        let elements = map lf constraints
            numcols = maximum $ length objective:map length elements
            (collb, colub) = (zero, dense $ replicate numcols inf)
            (rowlb, rowub) = dense *** dense $ unzip $ map bound constraints
        Solver.setObjSense direction
        Solver.loadProblem elements collb colub objective rowlb rowub
        status <- Solver.solve
        optimal <- Solver.isProvenOptimal
        case (status, optimal) of
            (Solver.Finished, True) -> (,) <$> Solver.getColSolution <*> Solver.getObjValue
            _ -> return (zero, 0.0)

data StandardConstraint = Lteq LinearFunction Double
    deriving Show

generalize :: StandardConstraint -> GeneralConstraint
generalize (Lteq f b) = Leq f b

data StandardForm = StandardForm Objective [StandardConstraint]
    deriving Show

instance LinearProgram StandardForm where
    solve (StandardForm objective constraints) =
        solve $ GeneralForm Solver.Maximize objective $ map generalize constraints
