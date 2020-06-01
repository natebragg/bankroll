module Numeric.Optimization.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,

    addRows,
    addColumns,

    LogLevel(..),
    setLogLevel,

    Pass(..),
    dual,

    module Bindings.Clp.Managed,
    module Solver,
    Solver.Solver(..)
) where

import Bindings.Clp.Managed (
    SimplexHandle,

    isProvenPrimalInfeasible,
    isProvenDualInfeasible,
    isPrimalObjectiveLimitReached,
    isDualObjectiveLimitReached,
    isIterationLimitReached,
    )
import qualified Bindings.Clp.Managed as Clp
import Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    coefficients,
    )

import Foreign.Ptr (nullPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Array (withArray, withArrayLen)
import System.IO.Unsafe (unsafePerformIO)
import Numeric.Optimization.Bankroll.Solver as Solver
import qualified Numeric.Optimization.Bankroll.Solver.Foreign as Foreign

version :: String
version = unsafePerformIO $ peekCString Clp.version

versionMajor :: Int
versionMajor = fromIntegral Clp.versionMajor

versionMinor :: Int
versionMinor = fromIntegral Clp.versionMinor

versionRelease :: Int
versionRelease = fromIntegral Clp.versionRelease

startsIndicesElements :: [LinearFunction] -> ([Int], [Int], [Double])
startsIndicesElements elematrix = (starts, concat indices, concat elements)
    where starts = scanl (+) 0 $ map length indices
          (indices, elements) = unzip $ map coefficients elematrix

addRows :: SimplexHandle -> [(Double, Double)] -> [LinearFunction] -> IO ()
addRows model bounds elematrix =
    let (rowLower, rowUpper) = unzip bounds
    in  withArrayLen (map realToFrac rowLower) $ \num_rows rowLower ->
        withArray (map realToFrac rowUpper) $ \rowUpper ->
            let addElements = Clp.addRows model (fromIntegral num_rows) rowLower rowUpper
            in  case elematrix of
                [] -> addElements nullPtr nullPtr nullPtr
                _ -> let (rowStarts, columns, elements) = startsIndicesElements elematrix
                     in  withArray (map fromIntegral rowStarts) $
                            withArray (map fromIntegral columns) .
                                (withArray (map realToFrac elements) .) . addElements

addColumns :: SimplexHandle -> [(Double, Double, Double)] -> [LinearFunction] -> IO ()
addColumns model bounds elematrix =
    let (columnLower, columnUpper, objective) = unzip3 bounds
    in  withArrayLen (map realToFrac columnLower) $ \num_cols columnLower ->
        withArray (map realToFrac columnUpper) $ \columnUpper ->
        withArray (map realToFrac objective) $ \objective ->
            let addElements = Clp.addColumns model (fromIntegral num_cols) columnLower columnUpper objective
            in case elematrix of
                [] -> addElements nullPtr nullPtr nullPtr
                _ -> let (columnStarts, rows, elements) = startsIndicesElements elematrix
                     in  withArray (map fromIntegral columnStarts) $
                            withArray (map fromIntegral rows) .
                                (withArray (map realToFrac elements) .) . addElements

data LogLevel = None | Final | Factorizations | PlusABitMore | Verbose
    deriving (Eq, Ord, Enum, Show)

setLogLevel :: SimplexHandle -> LogLevel -> IO ()
setLogLevel model level = Clp.setLogLevel model $ fromIntegral $ fromEnum level

data Pass = Initial
          | ValuesPass
          | Cleanup
    deriving (Eq, Ord, Enum, Show)

dual :: SimplexHandle -> Pass -> IO Status
dual model pass = fmap (toEnum . (3 +) . fromIntegral) $ Clp.dual model $ fromIntegral $ fromEnum pass

instance Foreign.Solver SimplexHandle where
    newModel = Clp.newModel >>= \m -> setLogLevel m None >> return m
    readMps = Clp.readMps
    getObjSense = Clp.getObjSense
    setObjSense = Clp.setObjSense
    getRowLower = Clp.getRowLower
    getRowUpper = Clp.getRowUpper
    getObjCoefficients = Clp.getObjCoefficients
    getColLower = Clp.getColLower
    getColUpper = Clp.getColUpper
    getNumElements = Clp.getNumElements
    getVectorStarts = Clp.getVectorStarts
    getIndices = Clp.getIndices
    getVectorLengths = Clp.getVectorLengths
    getElements = Clp.getElements
    getObjValue = Clp.getObjValue
    initialSolve = Clp.initialSolve
    getNumRows = Clp.getNumRows
    getNumCols = Clp.getNumCols
    isAbandoned = Clp.isAbandoned
    isProvenOptimal = Clp.isProvenOptimal
    getRowActivity = Clp.getRowActivity
    getColSolution = Clp.getColSolution

instance Solver SimplexHandle
