module Main where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import qualified Numeric.Optimization.Bankroll.Clp as Clp
import Numeric.Optimization.Bankroll.LinearFunction (dense, coordinates)
import Numeric.Optimization.Bankroll.Program (Solution, GeneralForm(..), (<=$), solve)
import Numeric.Optimization.Bankroll.Pretty (renderEqn)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.Printf (printf)

_DBL_MAX = encodeFloat s e
    where v = 1.0 :: Double
          r = floatRadix v
          d = floatDigits v
          e = (snd $ floatRange v) - d
          s = r ^ d - 1

input_by_columns :: Clp.SimplexSolver ()
input_by_columns =
    let rowBounds = [(0.0, 60.0),  -- Time
                     (0.0, 200.0), -- Lemons
                     (0.0, 250.0), -- Sugar
                     (0.0, 240.0), -- Water
                     (0.0, 50.0)]  -- Vodka
    in do
    Clp.addRows rowBounds []

    -- Regular
    let columnBounds = [(0.0, _DBL_MAX, 1.0)]
        elements = [dense [0.25, 1.0, 2.0, 2.0, 0.0]]
    Clp.addColumns columnBounds elements

    -- Special
    let columnBounds = [(0.0, _DBL_MAX, 2.0)]
        elements = [dense [0.5, 1.0, 1.25, 0.6, 0.5]]
    Clp.addColumns columnBounds elements

input_by_rows :: Clp.SimplexSolver ()
input_by_rows =
    let columnBounds = [(0.0, _DBL_MAX, 1.0), -- Regular
                        (0.0, _DBL_MAX, 2.0)] -- Special
    in do
    Clp.addColumns columnBounds []

    -- Time
    let rowBounds = [(0.0, 60.0)]
        elements = [dense [0.25, 0.5]]
    Clp.addRows rowBounds elements

    -- Lemons
    let rowBounds = [(0.0, 200.0)]
        elements = [dense [1.0, 1.0]]
    Clp.addRows rowBounds elements

    -- Sugar
    let rowBounds = [(0.0, 250.0)]
        elements = [dense [2.0, 1.25]]
    Clp.addRows rowBounds elements

    -- Water
    let rowBounds = [(0.0, 240.0)]
        elements = [dense [2.0, 0.6]]
    Clp.addRows rowBounds elements

    -- Vodka
    let rowBounds = [(0.0, 50.0)]
        elements = [dense [0.0, 0.5]]
    Clp.addRows rowBounds elements

input_by_problem :: Clp.SimplexSolver ()
input_by_problem =
    let --             Regular   Special
        vals = [dense [0.25,     0.5     ], -- Time
                dense [1.0,      1.0     ], -- Lemons
                dense [2.0,      1.25    ], -- Sugar
                dense [2.0,      0.6     ], -- Water
                dense [0.0,      0.5     ]] -- Vodka
        collb = dense [0.0,      0.0     ]
        colub = dense [_DBL_MAX, _DBL_MAX]
        obj   = dense [1.0,      2.0     ]
        --             Time  Lemons Sugar  Water  Vodka
        rowlb = dense [0.0,  0.0,   0.0,   0.0,   0.0 ]
        rowub = dense [60.0, 200.0, 250.0, 240.0, 50.0]
    in  Clp.loadProblem vals collb colub obj rowlb rowub

solve_general_form :: IO ()
solve_general_form =
    let --             Regular   Special
        obj =   dense [1.0,      2.0     ]
        cs  =  [dense [0.25,     0.5     ] <=$  60.0, -- Time
                dense [1.0,      1.0     ] <=$ 200.0, -- Lemons
                dense [2.0,      1.25    ] <=$ 250.0, -- Sugar
                dense [2.0,      0.6     ] <=$ 240.0, -- Water
                dense [0.0,      0.5     ] <=$  50.0] -- Vodka
        solver = solve (GeneralForm Clp.Maximize obj cs) :: Clp.SimplexSolver (Solution, Double)
        (s, v) = Clp.doSolver solver
    in  printf "Solution: %s = %s\n" (show v) (renderEqn ["Regular", "Special"] s)

input_by_file :: Clp.SimplexSolver ()
input_by_file = do
    status <- Clp.readMps "lemonade.mps" True False
    when (status /= 0) $
        liftIO $ exitWith $ ExitFailure status

main :: IO [()]
main = Clp.doSolverIO $ do
    Clp.setLogLevel Clp.None

    input_by_problem

    Clp.setObjSense Clp.Maximize

    status <- Clp.solve
    when (status /= Clp.Finished) $
        liftIO $ exitWith $ ExitFailure $ fromEnum status

    ipo   <- Clp.isProvenOptimal
    ippi  <- Clp.isProvenPrimalInfeasible
    ipdi  <- Clp.isProvenDualInfeasible
    ipolr <- Clp.isPrimalObjectiveLimitReached
    idolr <- Clp.isDualObjectiveLimitReached
    iilr  <- Clp.isIterationLimitReached
    ia    <- Clp.isAbandoned
    liftIO $ printf "Solution: opt %s, ppi %s, pdi %s, plr %s, dlr %s, ilr %s, abn %s\n"
       (show ipo) (show ippi) (show ipdi) (show ipolr) (show idolr) (show iilr) (show ia)

    pr <- Clp.getRowActivity
    forM (coordinates pr) $ \(row, pr_row) ->
        liftIO $ printf "row %d, value %f\n" row pr_row

    pc <- Clp.getColSolution
    forM (coordinates pc) $ \(col, pc_col) ->
        liftIO $ printf "col %d, solution %f\n" col pc_col
