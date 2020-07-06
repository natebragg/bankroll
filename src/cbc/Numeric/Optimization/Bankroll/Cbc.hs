{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.Optimization.Bankroll.Cbc (
    BranchCutSolver,
    Cbc.ModelHandle,

    getVersion,

    isInteger,
    setContinuous,
    setInteger,

    setParameter,

    isProvenInfeasible,
    isSolutionLimitReached,

    evalCbcSolverIO,
    evalCbcSolver,
    doCbcSolverIO,
    doCbcSolver,

    module Solver,
    Solver.Solver(..),
) where

import qualified Bindings.Cbc.Managed as Cbc

import Control.Monad.IO.Class (MonadIO(liftIO))
import Foreign.Ptr (Ptr)
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Array (peekArray, newArray)
import System.IO.Unsafe (unsafePerformIO)
import Numeric.Optimization.Bankroll.Solver as Solver
import qualified Numeric.Optimization.Bankroll.Solver.Foreign as Foreign

type BranchCutSolver = Solver Cbc.ModelHandle

evalCbcSolverIO :: BranchCutSolver a -> Cbc.ModelHandle -> IO a
evalCbcSolver :: BranchCutSolver a -> Cbc.ModelHandle -> a
doCbcSolverIO :: BranchCutSolver a -> IO a
doCbcSolver :: BranchCutSolver a -> a
(evalCbcSolverIO, evalCbcSolver, doCbcSolverIO, doCbcSolver) =
    (Solver.evalSolverIO, Solver.evalSolver, Solver.doSolverIO, Solver.doSolver)

withModel f = model >>= liftIO . f

getVersion :: String
getVersion = unsafePerformIO $ peekCString Cbc.getVersion

isInteger c            = withModel $ \m -> Cbc.isInteger     m $ fromIntegral c
setContinuous c        = withModel $ \m -> Cbc.setContinuous m $ fromIntegral c
setInteger c           = withModel $ \m -> Cbc.setInteger    m $ fromIntegral c

setParameter n v       = withModel $ \m -> withCString n $ \n -> withCString v $ \v -> Cbc.setParameter m n v

isProvenInfeasible     = withModel Cbc.isProvenInfeasible
isSolutionLimitReached = withModel Cbc.isSolutionLimitReached

-- This is not strictly correct because it is apparently possible for matrices
-- to contain junk, but the actual function is hidden.  Also it leaks memory.
gvlWorkaround :: BranchCutSolver (Ptr CInt)
gvlWorkaround = do
    nc <- fromIntegral <$> getNumCols
    vs <- map fromIntegral <$> (liftIO . peekArray (nc + 1) =<< Foreign.getVectorStarts)
    let vl = map (uncurry (-)) $ zip (tail vs) vs
    liftIO $ newArray vl

instance Foreign.MonadSolver BranchCutSolver where
    newModel           = liftIO Cbc.newModel >>= setModel
    loadProblem nc nr s i v cl cu o rl ru =
                         withModel $ \m -> Cbc.loadProblem m nc nr s i v cl cu o rl ru
    readMps f _ _      = withModel $ \m -> Cbc.readMps m f
    getObjSense        = withModel Cbc.getObjSense
    setObjSense s      = withModel $ \m -> Cbc.setObjSense m s
    getRowLower        = withModel Cbc.getRowLower
    getRowUpper        = withModel Cbc.getRowUpper
    getObjCoefficients = withModel Cbc.getObjCoefficients
    getColLower        = withModel Cbc.getColLower
    getColUpper        = withModel Cbc.getColUpper
    getNumElements     = withModel Cbc.getNumElements
    getVectorStarts    = withModel Cbc.getVectorStarts
    getIndices         = withModel Cbc.getIndices
    getVectorLengths   = gvlWorkaround
    getElements        = withModel Cbc.getElements
    getObjValue        = withModel Cbc.getObjValue
    initialSolve       = withModel Cbc.solve
    getNumRows         = withModel Cbc.getNumRows
    getNumCols         = withModel Cbc.getNumCols
    isAbandoned        = withModel Cbc.isAbandoned
    isProvenOptimal    = withModel Cbc.isProvenOptimal
    getRowActivity     = withModel Cbc.getRowActivity
    getColSolution     = withModel Cbc.getColSolution

instance MonadSolver BranchCutSolver where
    solve = Foreign.initialSolve >>= \case
                0 -> return Finished
                1 -> return Stopped
                2 -> return Errors
                5 -> return UserStopped
                _ -> return Unknown
