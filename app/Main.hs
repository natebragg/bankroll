module Main where

import Control.Monad (forM, when)
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

input_by_file :: Clp.SimplexHandle -> String -> IO ()
input_by_file model fn = do
    status <- Clp.readMps model fn True False
    when (status /= 0) $
        exitWith $ ExitFailure status

enumerate = uncurry zip . coefficients

main :: IO ()
main = do
    fn <- handleArgs
    model <- Clp.newModel
    Clp.setLogLevel model Clp.None

    input_by_file model fn

    Clp.setObjSense model Clp.Maximize

    status <- Clp.solve model
    when (status /= Clp.Optimal) $
        exitWith $ ExitFailure $ fromEnum status

    printf "Solution: opt %s, ppi %s, pdi %s, plr %s, dlr %s, ilr %s, abn %s\n"
       <$> (show <$> Clp.isProvenOptimal model)
       <*> (show <$> Clp.isProvenPrimalInfeasible model)
       <*> (show <$> Clp.isProvenDualInfeasible model)
       <*> (show <$> Clp.isPrimalObjectiveLimitReached model)
       <*> (show <$> Clp.isDualObjectiveLimitReached model)
       <*> (show <$> Clp.isIterationLimitReached model)
       <*> (show <$> Clp.isAbandoned model)
       >>= id

    pr <- Clp.getRowActivity model
    forM (enumerate pr) $ \(row, pr_row) ->
        printf "row %d, value %f\n" row pr_row

    pc <- Clp.getColSolution model
    forM (enumerate pc) $ \(col, pc_col) ->
        printf "col %d, solution %f\n" col pc_col

    return ()
