{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Cbc.Cbc (
    getVersion,

    Model,
    ModelHandle,

    newModel,
    deleteModel,

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..))

foreign import ccall unsafe "Cbc_getVersion"     getVersion     :: CString

data Model = Model
type ModelHandle = Ptr Model

foreign import ccall unsafe "Cbc_newModel"       newModel       :: IO ModelHandle
foreign import ccall unsafe "&Cbc_deleteModel"   deleteModel    :: FunPtr (ModelHandle -> IO ())

foreign import ccall unsafe "Cbc_loadProblem"    loadProblem    :: ModelHandle -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall unsafe "Cbc_readMps"        readMps        :: ModelHandle -> CString -> Bool -> Bool -> IO CInt

foreign import ccall unsafe "Cbc_getObjSense"                   getObjSense                   :: ModelHandle -> IO CDouble
foreign import ccall unsafe "Cbc_setObjSense"                   setObjSense                   :: ModelHandle -> CDouble -> IO ()
foreign import ccall unsafe "Cbc_getRowLower"                   getRowLower                   :: ModelHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Cbc_getRowUpper"                   getRowUpper                   :: ModelHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Cbc_getObjCoefficients"            getObjCoefficients            :: ModelHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Cbc_getColLower"                   getColLower                   :: ModelHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Cbc_getColUpper"                   getColUpper                   :: ModelHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Cbc_getNumElements"                getNumElements                :: ModelHandle -> IO CInt
foreign import ccall unsafe "Cbc_getIndices"                    getIndices                    :: ModelHandle -> IO (Ptr CInt)
foreign import ccall unsafe "Cbc_getElements"                   getElements                   :: ModelHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Cbc_getObjValue"                   getObjValue                   :: ModelHandle -> IO CDouble

foreign import ccall unsafe "Cbc_solve"          solve          :: ModelHandle -> IO CInt

foreign import ccall unsafe "Cbc_getNumRows"     getNumRows     :: ModelHandle -> IO CInt
foreign import ccall unsafe "Cbc_getNumCols"     getNumCols     :: ModelHandle -> IO CInt

foreign import ccall unsafe "Cbc_isAbandoned"                   isAbandoned                   :: ModelHandle -> IO Bool
foreign import ccall unsafe "Cbc_isProvenOptimal"               isProvenOptimal               :: ModelHandle -> IO Bool
foreign import ccall unsafe "Cbc_isProvenInfeasible"            isProvenInfeasible            :: ModelHandle -> IO Bool
foreign import ccall unsafe "Cbc_isSolutionLimitReached"        isSolutionLimitReached        :: ModelHandle -> IO Bool

foreign import ccall unsafe "Cbc_getRowActivity" getRowActivity :: ModelHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Cbc_getColSolution" getColSolution :: ModelHandle -> IO (Ptr CDouble)
