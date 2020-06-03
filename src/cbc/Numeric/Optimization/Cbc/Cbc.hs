{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeric.Optimization.Cbc.Cbc (
    BranchCutSolver,

    getVersion,

    isProvenInfeasible,
    isSolutionLimitReached,

    module Solver,
    Solver.Solver(..)
) where

import qualified Bindings.Cbc.Managed as Cbc

import Control.Monad.IO.Class (MonadIO(liftIO))
import Foreign.Ptr (Ptr)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Array (peekArray, newArray)
import System.IO.Unsafe (unsafePerformIO)
import Numeric.Optimization.Bankroll.Solver as Solver
import qualified Numeric.Optimization.Bankroll.Solver.Foreign as Foreign

type BranchCutSolver = Solver Cbc.ModelHandle

withModel f = model >>= liftIO . f

getVersion :: String
getVersion = unsafePerformIO $ peekCString Cbc.getVersion

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
    readMps f k i      = withModel $ \m -> Cbc.readMps m f k i
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

instance MonadSolver BranchCutSolver
