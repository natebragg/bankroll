module Main where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(RequireOrder))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(ExitFailure))
import Text.Printf (printf)
import Text.Read (readEither)

import qualified Numeric.Optimization.Clp.Clp as Bankroll
import qualified Numeric.Optimization.Cbc.Cbc as Bankroll
import Numeric.Optimization.Bankroll.LinearFunction (coefficients)
import PackageInfo (version, appName, synopsis)

data Flag = Version
          | Help
          | Solver String
    deriving Eq

data Solver = Clp
            | Cbc
    deriving (Show, Read, Ord, Bounded, Enum, Eq)

solverlist :: String
solverlist = intercalate ", " (map show [(Clp)..])

opts :: [OptDescr Flag]
opts = Option ['v'] ["version"] (NoArg Version) "Version information." :
       Option ['h'] ["help"] (NoArg Help) "Print this message." :
       Option ['s'] ["solver"] (ReqArg Solver "SOLVER") ("One of: " ++ solverlist) :
       []

printUsage :: IO ()
printUsage = putStr (usageInfo header opts)
    where header = "Usage: " ++ appName ++ " [-s <s>] filename.mps\n" ++ synopsis ++ "\n"

printVersion :: IO ()
printVersion = do
    putStrLn $ appName ++ ": " ++ synopsis
    putStrLn $ "Version " ++ version
    putStrLn $ "Using Clp version " ++ Bankroll.version
    putStrLn $ "Using Cbc version " ++ Bankroll.getVersion

printErrors :: [String] -> IO ()
printErrors es = putStr (concat es) >> printUsage

handleArgs :: IO (Solver, String)
handleArgs = do
    args <- getArgs
    case getOpt RequireOrder opts args of
        (os,  _, [])
            | Help    `elem` os -> printUsage >> exitSuccess
            | Version `elem` os -> printVersion >> exitSuccess
        ( _, [], es) -> printErrors ("Filename required\n":es) >> exitFailure
        ( _, _:_:_, es) -> printErrors ("Args not recognized\n":es) >> exitFailure
        ( _, _, es@(_:_)) -> printErrors es >> exitFailure
        (os, [fn], []) -> do
            let (ses, ss) = partitionEithers [readEither m | Solver m <- os]
            when (not $ null ses) $ do
                putStrLn $ "Option solver requires one of: " ++ solverlist
                printUsage >> exitFailure
            return (last $ Clp:ss, fn)

solveMps :: Bankroll.MonadSolver m => String -> m ()
solveMps fn = do
    status <- Bankroll.readMps fn True False
    when (status /= 0) $
        liftIO $ exitWith $ ExitFailure status

    Bankroll.setObjSense Bankroll.Maximize

    status <- Bankroll.solve
    liftIO $ when (status /= Bankroll.Optimal) $ do
        putStrLn "Solver did not finish"
        exitWith $ ExitFailure $ fromEnum status

    optimal <- Bankroll.isProvenOptimal
    liftIO $ when (not optimal) $ do
        putStrLn "Solver could not find optimal solution"
        exitWith $ ExitFailure $ fromEnum status

    pr <- Bankroll.getRowActivity
    forM (enumerate pr) $ \(row, pr_row) ->
        liftIO $ printf "row %d, value %f\n" row pr_row

    pc <- Bankroll.getColSolution
    forM (enumerate pc) $ \(col, pc_col) ->
        liftIO $ printf "col %d, solution %f\n" col pc_col

    return ()
    where enumerate = uncurry zip . coefficients

main :: IO ()
main = do
    (solver, fn) <- liftIO handleArgs
    case solver of
        Clp -> Bankroll.doSolverIO (solveMps fn :: Bankroll.SimplexSolver ())
        Cbc -> Bankroll.doSolverIO (solveMps fn :: Bankroll.BranchCutSolver ())
