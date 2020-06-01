module Numeric.Optimization.Bankroll.Solver.Foreign (
    Solver(..)
) where

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..))

class Solver s where
    newModel :: IO s
    readMps :: s -> CString -> Bool -> Bool -> IO CInt
    getObjSense :: s -> IO CDouble
    setObjSense :: s -> CDouble -> IO ()
    getRowLower :: s -> IO (Ptr CDouble)
    getRowUpper :: s -> IO (Ptr CDouble)
    getObjCoefficients :: s -> IO (Ptr CDouble)
    getColLower :: s -> IO (Ptr CDouble)
    getColUpper :: s -> IO (Ptr CDouble)
    getNumElements :: s -> IO CInt
    getVectorStarts :: s -> IO (Ptr CInt)
    getIndices :: s -> IO (Ptr CInt)
    getVectorLengths :: s -> IO (Ptr CInt)
    getElements :: s -> IO (Ptr CDouble)
    getObjValue :: s -> IO CDouble
    initialSolve :: s -> IO CInt
    getNumRows :: s -> IO CInt
    getNumCols :: s -> IO CInt
    isAbandoned :: s -> IO Bool
    isProvenOptimal :: s -> IO Bool
    getRowActivity :: s -> IO (Ptr CDouble)
    getColSolution :: s -> IO (Ptr CDouble)
