{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.Optimization.Bankroll.Clp (
    SimplexSolver,

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

    isProvenPrimalInfeasible,
    isProvenDualInfeasible,
    isPrimalObjectiveLimitReached,
    isDualObjectiveLimitReached,
    isIterationLimitReached,

    module Solver,
    Solver.Solver(..)
) where

import qualified Bindings.Clp.Managed as Clp
import Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    coefficientOffsets,
    )

import Control.Monad.IO.Class (MonadIO(liftIO))
import Foreign.Ptr (nullPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (newArray)
import System.IO.Unsafe (unsafePerformIO)
import Numeric.Optimization.Bankroll.Solver as Solver
import qualified Numeric.Optimization.Bankroll.Solver.Foreign as Foreign

type SimplexSolver = Solver Clp.SimplexHandle

withModel f = model >>= liftIO . f

version :: String
version = unsafePerformIO $ peekCString Clp.version

versionMajor :: Int
versionMajor = fromIntegral Clp.versionMajor

versionMinor :: Int
versionMinor = fromIntegral Clp.versionMinor

versionRelease :: Int
versionRelease = fromIntegral Clp.versionRelease

addRows :: [(Double, Double)] -> [LinearFunction] -> SimplexSolver ()
addRows bounds values = do
    let (rowlb, rowub) = unzip bounds
        numrows = fromIntegral $ length bounds
        (start, index, value) = coefficientOffsets values
        newArrayOrNull xs = if null xs then pure nullPtr else liftIO $ newArray xs
    rowlb <- newArrayOrNull (map realToFrac   rowlb)
    rowub <- newArrayOrNull (map realToFrac   rowub)
    start <- newArrayOrNull (map fromIntegral start)
    index <- newArrayOrNull (map fromIntegral index)
    value <- newArrayOrNull (map realToFrac   value)
    withModel $ \m -> Clp.addRows m numrows rowlb rowub start index value
    liftIO $ do free rowlb; free rowub; free start; free index; free value

addColumns :: [(Double, Double, Double)] -> [LinearFunction] -> SimplexSolver ()
addColumns bounds values = do
    let (collb, colub, obj) = unzip3 bounds
        (start, index, value) = coefficientOffsets values
        numcols = fromIntegral $ length bounds
        newArrayOrNull xs = if null xs then pure nullPtr else liftIO $ newArray xs
    collb <- newArrayOrNull (map realToFrac   collb)
    colub <- newArrayOrNull (map realToFrac   colub)
    obj   <- newArrayOrNull (map realToFrac   obj  )
    start <- newArrayOrNull (map fromIntegral start)
    index <- newArrayOrNull (map fromIntegral index)
    value <- newArrayOrNull (map realToFrac   value)
    withModel $ \m -> Clp.addColumns m numcols collb colub obj start index value
    liftIO $ do free collb; free colub; free obj; free start; free index; free value

data LogLevel = None | Final | Factorizations | PlusABitMore | Verbose
    deriving (Eq, Ord, Enum, Show)

setLogLevel :: LogLevel -> SimplexSolver ()
setLogLevel level = withModel $ \m -> Clp.setLogLevel m $ fromIntegral $ fromEnum level

data Pass = Initial
          | ValuesPass
          | Cleanup
    deriving (Eq, Ord, Enum, Show)

dual :: Pass -> SimplexSolver Status
dual pass = withModel $ \m -> fmap (toEnum . (3 +) . fromIntegral) $ Clp.dual m $ fromIntegral $ fromEnum pass

isProvenPrimalInfeasible      = withModel Clp.isProvenPrimalInfeasible
isProvenDualInfeasible        = withModel Clp.isProvenDualInfeasible
isPrimalObjectiveLimitReached = withModel Clp.isPrimalObjectiveLimitReached
isDualObjectiveLimitReached   = withModel Clp.isDualObjectiveLimitReached
isIterationLimitReached       = withModel Clp.isIterationLimitReached

instance Foreign.MonadSolver SimplexSolver where
    newModel           = liftIO Clp.newModel >>= setModel >> setLogLevel None
    loadProblem nc nr s i v cl cu o rl ru =
                         withModel $ \m -> Clp.loadProblem m nc nr s i v cl cu o rl ru
    readMps f k i      = withModel $ \m -> Clp.readMps m f k i
    getObjSense        = withModel Clp.getObjSense
    setObjSense s      = withModel $ \m -> Clp.setObjSense m s
    getRowLower        = withModel Clp.getRowLower
    getRowUpper        = withModel Clp.getRowUpper
    getObjCoefficients = withModel Clp.getObjCoefficients
    getColLower        = withModel Clp.getColLower
    getColUpper        = withModel Clp.getColUpper
    getNumElements     = withModel Clp.getNumElements
    getVectorStarts    = withModel Clp.getVectorStarts
    getIndices         = withModel Clp.getIndices
    getVectorLengths   = withModel Clp.getVectorLengths
    getElements        = withModel Clp.getElements
    getObjValue        = withModel Clp.getObjValue
    initialSolve       = withModel Clp.initialSolve
    getNumRows         = withModel Clp.getNumRows
    getNumCols         = withModel Clp.getNumCols
    isAbandoned        = withModel Clp.isAbandoned
    isProvenOptimal    = withModel Clp.isProvenOptimal
    getRowActivity     = withModel Clp.getRowActivity
    getColSolution     = withModel Clp.getColSolution

instance MonadSolver SimplexSolver where
    solve = Foreign.initialSolve >>= \case
                0 -> return Finished
                1 -> return Finished
                2 -> return Finished
                3 -> return Stopped
                4 -> return Errors
                5 -> return UserStopped
                _ -> return Unknown
