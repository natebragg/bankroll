module Numeric.Optimization.Bankroll.Solver.Foreign (
    MonadSolver(..),
) where

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..))

class Monad m => MonadSolver m where
    newModel :: m ()
    loadProblem :: CInt -> CInt -> Ptr CInt -> Ptr CInt ->
                   Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
                   Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> m ()
    readMps :: CString -> Bool -> Bool -> m CInt
    getObjSense :: m CDouble
    setObjSense :: CDouble -> m ()
    getRowLower :: m (Ptr CDouble)
    getRowUpper :: m (Ptr CDouble)
    getObjCoefficients :: m (Ptr CDouble)
    getColLower :: m (Ptr CDouble)
    getColUpper :: m (Ptr CDouble)
    getNumElements :: m CInt
    getVectorStarts :: m (Ptr CInt)
    getIndices :: m (Ptr CInt)
    getVectorLengths :: m (Ptr CInt)
    getElements :: m (Ptr CDouble)
    getObjValue :: m CDouble
    initialSolve :: m CInt
    getNumRows :: m CInt
    getNumCols :: m CInt
    isAbandoned :: m Bool
    isProvenOptimal :: m Bool
    getRowActivity :: m (Ptr CDouble)
    getColSolution :: m (Ptr CDouble)
