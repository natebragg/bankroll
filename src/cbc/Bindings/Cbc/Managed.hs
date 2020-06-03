module Bindings.Cbc.Managed (
    getVersion,

    ModelHandle,

    newModel,

    loadProblem,
    readMps,

    getObjSense,
    setObjSense,
    getRowLower,
    getRowUpper,
    getObjCoefficients,
    getColLower,
    getColUpper,
    getNumElements,
    getVectorStarts,
    getIndices,
    getElements,
    getObjValue,

    solve,

    getNumRows,
    getNumCols,

    isAbandoned,
    isProvenOptimal,
    isProvenInfeasible,
    isSolutionLimitReached,

    getRowActivity,
    getColSolution,
) where

import Bindings.Cbc.Cbc (
    getVersion,
    )
import qualified Bindings.Cbc.Cbc as Unmanaged

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)

newtype ModelHandle = ModelHandle { runModel :: ForeignPtr Unmanaged.Model }

newModel :: IO ModelHandle
newModel = Unmanaged.newModel >>= newForeignPtr Unmanaged.deleteModel >>= pure . ModelHandle

loadProblem :: ModelHandle -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
loadProblem model numcols numrows start index value collb colub obj rowlb rowub = withForeignPtr (runModel model) $ \model ->
    Unmanaged.loadProblem model numcols numrows start index value collb colub obj rowlb rowub

readMps :: ModelHandle -> CString -> Bool -> Bool -> IO CInt
readMps model filename keepNames ignoreErrors = withForeignPtr (runModel model) $ \model ->
    Unmanaged.readMps model filename keepNames ignoreErrors

getObjSense :: ModelHandle -> IO CDouble
getObjSense model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getObjSense model

setObjSense :: ModelHandle -> CDouble -> IO ()
setObjSense model value = withForeignPtr (runModel model) $ \model ->
    Unmanaged.setObjSense model value

getRowLower :: ModelHandle -> IO (Ptr CDouble)
getRowLower model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getRowLower model

getRowUpper :: ModelHandle -> IO (Ptr CDouble)
getRowUpper model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getRowUpper model

getObjCoefficients :: ModelHandle -> IO (Ptr CDouble)
getObjCoefficients model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getObjCoefficients model

getColLower :: ModelHandle -> IO (Ptr CDouble)
getColLower model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getColLower model

getColUpper :: ModelHandle -> IO (Ptr CDouble)
getColUpper model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getColUpper model

getNumElements :: ModelHandle -> IO CInt
getNumElements model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getNumElements model

getVectorStarts :: ModelHandle -> IO (Ptr CInt)
getVectorStarts model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getVectorStarts model

getIndices :: ModelHandle -> IO (Ptr CInt)
getIndices model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getIndices model

getElements :: ModelHandle -> IO (Ptr CDouble)
getElements model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getElements model

getObjValue :: ModelHandle -> IO CDouble
getObjValue model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getObjValue model

solve :: ModelHandle -> IO CInt
solve model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.solve model

getNumRows :: ModelHandle -> IO CInt
getNumRows model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getNumRows model

getNumCols :: ModelHandle -> IO CInt
getNumCols model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getNumCols model

isAbandoned :: ModelHandle -> IO Bool
isAbandoned model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isAbandoned model

isProvenOptimal :: ModelHandle -> IO Bool
isProvenOptimal model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isProvenOptimal model

isProvenInfeasible :: ModelHandle -> IO Bool
isProvenInfeasible model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isProvenInfeasible model

isSolutionLimitReached :: ModelHandle -> IO Bool
isSolutionLimitReached model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isSolutionLimitReached model

getRowActivity :: ModelHandle -> IO (Ptr CDouble)
getRowActivity model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getRowActivity model

getColSolution :: ModelHandle -> IO (Ptr CDouble)
getColSolution model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getColSolution model
