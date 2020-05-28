module Data.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,

    SimplexHandle,

    newModel,

    readMps,
    addRows,
    addColumns,

    OptimizationDirection(..),
    optimizationDirection,
    setOptimizationDirection,
    rowBounds,
    columnBounds,
    getElements,
    objectiveValue,
    LogLevel(..),
    setLogLevel,

    Status(..),
    initialSolve,
    Pass(..),
    dual,

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

import Data.Clp.Managed (
    SimplexHandle,
    newModel,

    isAbandoned,
    isProvenOptimal,
    isProvenPrimalInfeasible,
    isProvenDualInfeasible,
    isPrimalObjectiveLimitReached,
    isDualObjectiveLimitReached,
    isIterationLimitReached,
    )
import qualified Data.Clp.Managed as Clp
import Data.Clp.LinearFunction (
    LinearFunction,
    unpack,
    coefficients,
    )

import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Array (peekArray, withArray, withArrayLen)
import System.IO.Unsafe (unsafePerformIO)

version :: String
version = unsafePerformIO $ peekCString Clp.version

versionMajor :: Int
versionMajor = fromIntegral Clp.versionMajor

versionMinor :: Int
versionMinor = fromIntegral Clp.versionMinor

versionRelease :: Int
versionRelease = fromIntegral Clp.versionRelease

readMps :: SimplexHandle -> String -> Bool -> Bool -> IO Int
readMps model fn keepNames ignoreErrors =
    withCString fn $ \fn -> fromIntegral <$> Clp.readMps model fn keepNames ignoreErrors

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

data OptimizationDirection = Maximize | Ignore | Minimize
    deriving (Eq, Ord, Enum, Show)

optimizationDirection :: SimplexHandle -> IO OptimizationDirection
optimizationDirection model = toEnum <$> truncate <$> (1.0 +) <$> Clp.optimizationDirection model

setOptimizationDirection :: SimplexHandle -> OptimizationDirection -> IO ()
setOptimizationDirection model dir = Clp.setOptimizationDirection model $ (fromIntegral $ fromEnum dir) - 1.0

rowBounds :: SimplexHandle -> IO [(Double, Double)]
rowBounds model = do
    nr <- fromIntegral <$> Clp.getNumRows model
    rl <- map realToFrac <$> (peekArray nr =<< Clp.rowLower model)
    ru <- map realToFrac <$> (peekArray nr =<< Clp.rowUpper model)
    return $ zip rl ru

columnBounds :: SimplexHandle -> IO [(Double, Double, Double)]
columnBounds model = do
    nc <- fromIntegral <$> Clp.getNumCols model
    cl <- map realToFrac <$> (peekArray nc =<< Clp.columnLower model)
    cu <- map realToFrac <$> (peekArray nc =<< Clp.columnUpper model)
    ob <- map realToFrac <$> (peekArray nc =<< Clp.objective model)
    return $ zip3 cl cu ob

segment :: [Int] -> [a] -> [[a]]
segment [] [] = []
segment [] as = [as]
segment (n:ls) as = a:segment ls as'
    where (a, as') = splitAt n as

getElements :: SimplexHandle -> IO [[Double]]
getElements model = do
    ne <- fromIntegral <$> Clp.getNumElements model
    is <- map fromIntegral <$> (peekArray ne =<< Clp.getIndices model)
    es <- map realToFrac <$> (peekArray ne =<< Clp.getElements model)
    nc <- fromIntegral <$> Clp.getNumCols model
    vl <- map fromIntegral <$> (peekArray nc =<< Clp.getVectorLengths model)
    return $ map unpack $ segment vl (zip is es)

objectiveValue :: SimplexHandle -> IO Double
objectiveValue = (fmap realToFrac) . Clp.objectiveValue

data LogLevel = None | Final | Factorizations | PlusABitMore | Verbose
    deriving (Eq, Ord, Enum, Show)

setLogLevel :: SimplexHandle -> LogLevel -> IO ()
setLogLevel model level = Clp.setLogLevel model $ fromIntegral $ fromEnum level

data Status = Event3
            | Event2
            | Unknown
            | Optimal
            | PrimalInfeasible
            | DualInfeasible
            | Stopped
            | Errors
            | UserStopped
    deriving (Eq, Ord, Enum, Show)

initialSolve :: SimplexHandle -> IO Status
initialSolve model = fmap (toEnum . (3 +) . fromIntegral) $ Clp.initialSolve model

data Pass = Initial
          | ValuesPass
          | Cleanup
    deriving (Eq, Ord, Enum, Show)

dual :: SimplexHandle -> Pass -> IO Status
dual model pass = fmap (toEnum . (3 +) . fromIntegral) $ Clp.dual model $ fromIntegral $ fromEnum pass

getNumRows :: SimplexHandle -> IO Int
getNumRows = (fmap fromIntegral) . Clp.getNumRows

getNumCols :: SimplexHandle -> IO Int
getNumCols = (fmap fromIntegral) . Clp.getNumCols

getRowActivity :: SimplexHandle -> IO [Double]
getRowActivity model = do
    nr <- fromIntegral <$> Clp.getNumRows model
    map realToFrac <$> (peekArray nr =<< Clp.getRowActivity model)

getColSolution :: SimplexHandle -> IO [Double]
getColSolution model = do
    nc <- fromIntegral <$> Clp.getNumCols model
    map realToFrac <$> (peekArray nc =<< Clp.getColSolution model)
