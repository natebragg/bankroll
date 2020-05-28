module Data.Clp.Program (
    LinearProgram(..),
    Solution,
    Objective,
    LinearFunction,
    GeneralConstraint(..), (<=$), (==$), (>=$),
    GeneralForm(..),
    StandardConstraint(..),
    StandardForm(..),
) where

import qualified Data.Clp.Clp as Clp
import Data.Clp.LinearFunction (LinearFunction, dense)

import Data.Foldable (toList)
import Numeric.Algebra (zero)
import System.IO.Unsafe (unsafePerformIO)

inf = read "Infinity"

type Solution = LinearFunction

class LinearProgram a where
    solve :: a -> (Solution, Double)

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

data GeneralForm = GeneralForm Clp.OptimizationDirection Objective [GeneralConstraint]
    deriving Show

instance LinearProgram GeneralForm where
    solve (GeneralForm direction objective constraints) =
        let elements = map lf constraints
            row_length = maximum $ 0:map length elements
            obj_dec = toList objective
            columnBounds = [(0.0, inf, obj) | obj <- obj_dec] ++
                           replicate (row_length - length obj_dec) (0.0, inf, 0.0)
            rowBounds = map bound constraints
        in  unsafePerformIO $ do
        model <- Clp.newModel
        Clp.setLogLevel model Clp.None
        Clp.setOptimizationDirection model direction
        Clp.addColumns model columnBounds []
        Clp.addRows model rowBounds elements
        status <- Clp.initialSolve model
        case status of
            Clp.Optimal -> (,) <$> (dense <$> Clp.getColSolution model) <*> Clp.objectiveValue model
            _ -> return (zero, 0.0)

data StandardConstraint = Lteq LinearFunction Double
    deriving Show

generalize :: StandardConstraint -> GeneralConstraint
generalize (Lteq f b) = Leq f b

data StandardForm = StandardForm Objective [StandardConstraint]
    deriving Show

instance LinearProgram StandardForm where
    solve (StandardForm objective constraints) =
        solve $ GeneralForm Clp.Maximize objective $ map generalize constraints
