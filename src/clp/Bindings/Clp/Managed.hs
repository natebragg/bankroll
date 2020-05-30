module Bindings.Clp.Managed (
    version,
    versionMajor,
    versionMinor,
    versionRelease,

    SimplexHandle,

    newModel,

    readMps,
    addRows,
    addColumns,

    getObjSense,
    setObjSense,
    getRowLower,
    getRowUpper,
    getObjCoefficients,
    getColLower,
    getColUpper,
    getNumElements,
    getIndices,
    getVectorLengths,
    getElements,
    getObjValue,
    setLogLevel,

    initialSolve,
    dual,

    getNumRows,
    getNumCols,

    isAbandoned,
    isProvenOptimal,
    isProvenPrimalInfeasible,
    isProvenDualInfeasible,
    isPrimalObjectiveLimitReached,
    isDualObjectiveLimitReached,
    isIterationLimitReached,

    getRowActivity,
    getColSolution,
) where

import Bindings.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,
    )
import qualified Bindings.Clp.Clp as Unmanaged

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)

newtype SimplexHandle = SimplexHandle { runModel :: ForeignPtr Unmanaged.Simplex }

newModel :: IO SimplexHandle
newModel = Unmanaged.newModel >>= newForeignPtr Unmanaged.deleteModel >>= pure . SimplexHandle

readMps :: SimplexHandle -> CString -> Bool -> Bool -> IO CInt
readMps model filename keepNames ignoreErrors = withForeignPtr (runModel model) $ \model ->
    Unmanaged.readMps model filename keepNames ignoreErrors

addRows :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
addRows model number rowLower rowUpper rowStarts columns elements = withForeignPtr (runModel model) $ \model ->
    Unmanaged.addRows model number rowLower rowUpper rowStarts columns elements

addColumns :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
addColumns model number columnLower columnUpper objective columnStarts rows elements = withForeignPtr (runModel model) $ \model ->
    Unmanaged.addColumns model number columnLower columnUpper objective columnStarts rows elements

getObjSense :: SimplexHandle -> IO CDouble
getObjSense model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getObjSense model

setObjSense :: SimplexHandle -> CDouble -> IO ()
setObjSense model value = withForeignPtr (runModel model) $ \model ->
    Unmanaged.setObjSense model value

getRowLower :: SimplexHandle -> IO (Ptr CDouble)
getRowLower model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getRowLower model

getRowUpper :: SimplexHandle -> IO (Ptr CDouble)
getRowUpper model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getRowUpper model

getObjCoefficients :: SimplexHandle -> IO (Ptr CDouble)
getObjCoefficients model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getObjCoefficients model

getColLower :: SimplexHandle -> IO (Ptr CDouble)
getColLower model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getColLower model

getColUpper :: SimplexHandle -> IO (Ptr CDouble)
getColUpper model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getColUpper model

getNumElements :: SimplexHandle -> IO CInt
getNumElements model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getNumElements model

getIndices :: SimplexHandle -> IO (Ptr CInt)
getIndices model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getIndices model

getVectorLengths :: SimplexHandle -> IO (Ptr CInt)
getVectorLengths model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getVectorLengths model

getElements :: SimplexHandle -> IO (Ptr CDouble)
getElements model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getElements model

getObjValue :: SimplexHandle -> IO CDouble
getObjValue model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getObjValue model

setLogLevel :: SimplexHandle -> CInt -> IO ()
setLogLevel model value = withForeignPtr (runModel model) $ \model ->
    Unmanaged.setLogLevel model value

initialSolve :: SimplexHandle -> IO CInt
initialSolve model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.initialSolve model

dual :: SimplexHandle -> CInt -> IO CInt
dual model pass = withForeignPtr (runModel model) $ \model ->
    Unmanaged.dual model pass

getNumRows :: SimplexHandle -> IO CInt
getNumRows model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getNumRows model

getNumCols :: SimplexHandle -> IO CInt
getNumCols model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getNumCols model

isAbandoned :: SimplexHandle -> IO Bool
isAbandoned model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isAbandoned model

isProvenOptimal :: SimplexHandle -> IO Bool
isProvenOptimal model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isProvenOptimal model

isProvenPrimalInfeasible :: SimplexHandle -> IO Bool
isProvenPrimalInfeasible model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isProvenPrimalInfeasible model

isProvenDualInfeasible :: SimplexHandle -> IO Bool
isProvenDualInfeasible model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isProvenDualInfeasible model

isPrimalObjectiveLimitReached :: SimplexHandle -> IO Bool
isPrimalObjectiveLimitReached model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isPrimalObjectiveLimitReached model

isDualObjectiveLimitReached :: SimplexHandle -> IO Bool
isDualObjectiveLimitReached model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isDualObjectiveLimitReached model

isIterationLimitReached :: SimplexHandle -> IO Bool
isIterationLimitReached model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.isIterationLimitReached model

getRowActivity :: SimplexHandle -> IO (Ptr CDouble)
getRowActivity model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getRowActivity model

getColSolution :: SimplexHandle -> IO (Ptr CDouble)
getColSolution model = withForeignPtr (runModel model) $ \model ->
    Unmanaged.getColSolution model
