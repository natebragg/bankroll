module Bindings.Cbc.Managed (
    getVersion,

    ModelHandle,

    newModel,

    readMps,

    getObjSense,
    setObjSense,
    getRowLower,
    getRowUpper,
    getObjCoefficients,
    getColLower,
    getColUpper,
    getNumElements,
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

type ModelHandle = ForeignPtr Unmanaged.Model

newModel :: IO ModelHandle
newModel = Unmanaged.newModel >>= newForeignPtr Unmanaged.deleteModel

readMps :: ModelHandle -> CString -> Bool -> Bool -> IO CInt
readMps model filename keepNames ignoreErrors = withForeignPtr model $ \model ->
    Unmanaged.readMps model filename keepNames ignoreErrors

getObjSense :: ModelHandle -> IO CDouble
getObjSense model = withForeignPtr model $ \model ->
    Unmanaged.getObjSense model

setObjSense :: ModelHandle -> CDouble -> IO ()
setObjSense model value = withForeignPtr model $ \model ->
    Unmanaged.setObjSense model value

getRowLower :: ModelHandle -> IO (Ptr CDouble)
getRowLower model = withForeignPtr model $ \model ->
    Unmanaged.getRowLower model

getRowUpper :: ModelHandle -> IO (Ptr CDouble)
getRowUpper model = withForeignPtr model $ \model ->
    Unmanaged.getRowUpper model

getObjCoefficients :: ModelHandle -> IO (Ptr CDouble)
getObjCoefficients model = withForeignPtr model $ \model ->
    Unmanaged.getObjCoefficients model

getColLower :: ModelHandle -> IO (Ptr CDouble)
getColLower model = withForeignPtr model $ \model ->
    Unmanaged.getColLower model

getColUpper :: ModelHandle -> IO (Ptr CDouble)
getColUpper model = withForeignPtr model $ \model ->
    Unmanaged.getColUpper model

getNumElements :: ModelHandle -> IO CInt
getNumElements model = withForeignPtr model $ \model ->
    Unmanaged.getNumElements model

getIndices :: ModelHandle -> IO (Ptr CInt)
getIndices model = withForeignPtr model $ \model ->
    Unmanaged.getIndices model

getElements :: ModelHandle -> IO (Ptr CDouble)
getElements model = withForeignPtr model $ \model ->
    Unmanaged.getElements model

getObjValue :: ModelHandle -> IO CDouble
getObjValue model = withForeignPtr model $ \model ->
    Unmanaged.getObjValue model

solve :: ModelHandle -> IO CInt
solve model = withForeignPtr model $ \model ->
    Unmanaged.solve model

getNumRows :: ModelHandle -> IO CInt
getNumRows model = withForeignPtr model $ \model ->
    Unmanaged.getNumRows model

getNumCols :: ModelHandle -> IO CInt
getNumCols model = withForeignPtr model $ \model ->
    Unmanaged.getNumCols model

isAbandoned :: ModelHandle -> IO Bool
isAbandoned model = withForeignPtr model $ \model ->
    Unmanaged.isAbandoned model

isProvenOptimal :: ModelHandle -> IO Bool
isProvenOptimal model = withForeignPtr model $ \model ->
    Unmanaged.isProvenOptimal model

isProvenInfeasible :: ModelHandle -> IO Bool
isProvenInfeasible model = withForeignPtr model $ \model ->
    Unmanaged.isProvenInfeasible model

isSolutionLimitReached :: ModelHandle -> IO Bool
isSolutionLimitReached model = withForeignPtr model $ \model ->
    Unmanaged.isSolutionLimitReached model

getRowActivity :: ModelHandle -> IO (Ptr CDouble)
getRowActivity model = withForeignPtr model $ \model ->
    Unmanaged.getRowActivity model

getColSolution :: ModelHandle -> IO (Ptr CDouble)
getColSolution model = withForeignPtr model $ \model ->
    Unmanaged.getColSolution model
