module Main where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(RequireOrder))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(ExitFailure))
import Text.Printf (printf)

import qualified Numeric.Optimization.Clp.Clp as Clp
import Numeric.Optimization.Bankroll.LinearFunction (coefficients)
import PackageInfo (version, appName, synopsis)

data Flag = Version
          | Help
    deriving Eq

opts :: [OptDescr Flag]
opts = Option ['v'] ["version"] (NoArg Version) "Version information." :
       Option ['h'] ["help"] (NoArg Help) "Print this message." :
       []

printUsage :: IO ()
printUsage = putStr (usageInfo header opts)
    where header = "Usage: " ++ appName ++ " filename.mps\n" ++ synopsis ++ "\n"

printVersion :: IO ()
printVersion = do
    putStrLn $ appName ++ ": " ++ synopsis
    putStrLn $ "Version " ++ version
    putStrLn $ "Using Clp version " ++ Clp.version

printErrors :: [String] -> IO ()
printErrors es = putStr (concat es) >> printUsage

handleArgs :: IO String
handleArgs = do
    args <- getArgs
    case getOpt RequireOrder opts args of
        (os,  _, [])
            | Help    `elem` os -> printUsage >> exitSuccess
            | Version `elem` os -> printVersion >> exitSuccess
        ( _, [], es) -> printErrors ("Filename required\n":es) >> exitFailure
        ( _, _:_:_, es) -> printErrors ("Args not recognized\n":es) >> exitFailure
        ( _, _, es@(_:_)) -> printErrors es >> exitFailure
        (os, [fn], []) -> return fn

enumerate = uncurry zip . coefficients

main :: IO ()
main = Clp.doSolverIO $ do
    fn <- liftIO handleArgs
    Clp.newModel
    Clp.setLogLevel Clp.None

    status <- Clp.readMps fn True False
    when (status /= 0) $
        liftIO $ exitWith $ ExitFailure status

    Clp.setObjSense Clp.Maximize

    status <- Clp.solve
    when (status /= Clp.Optimal) $
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
    forM (enumerate pr) $ \(row, pr_row) ->
        liftIO $ printf "row %d, value %f\n" row pr_row

    pc <- Clp.getColSolution
    forM (enumerate pc) $ \(col, pc_col) ->
        liftIO $ printf "col %d, solution %f\n" col pc_col

    return ()
