module Numeric.Optimization.Bankroll.Solver (
    Foreign.newModel,
    Foreign.isAbandoned,
    Foreign.isProvenOptimal,

    Solver(..),
    OptimizationDirection(..),
    Status(..),
) where

import Data.Foldable (toList)
import Foreign.Ptr (nullPtr)
import Foreign.C.String (withCString)
import Foreign.Marshal.Array (peekArray, withArray)
import Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    dense,
    sparse,
    coefficientOffsets,
    transpose,
    )
import qualified Numeric.Optimization.Bankroll.Solver.Foreign as Foreign

data OptimizationDirection = Maximize | Ignore | Minimize
    deriving (Eq, Ord, Enum, Show)

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

class Foreign.Solver s => Solver s where
    readMps :: s -> String -> Bool -> Bool -> IO Int
    readMps model fn keepNames ignoreErrors =
        withCString fn $ \fn -> fromIntegral <$> Foreign.readMps model fn keepNames ignoreErrors

    loadProblem :: s -> [LinearFunction] -> LinearFunction -> LinearFunction -> LinearFunction -> LinearFunction -> LinearFunction -> IO ()
    loadProblem model values collb colub obj rowlb rowub =
        let (start, index, value) = coefficientOffsets $ transpose values -- loadProblem expects columns, but values is a list of rows
            [clbc, cubc, objc, rlbc, rubc] = map length [collb, colub, obj, rowlb, rowub]
            numcols = maximum $ clbc:cubc:objc:map length values
            numrows = maximum $ [rlbc, rubc, length values]
            pad n f = if null f then [] else toList f ++ replicate n 0.0
            withArrayOrNull xs = if null xs then ($ nullPtr) else withArray xs
        in  withArrayOrNull (map fromIntegral                        start) $ \start ->
            withArrayOrNull (map fromIntegral                        index) $ \index ->
            withArrayOrNull (map realToFrac                          value) $ \value ->
            withArrayOrNull (map realToFrac   $ pad (numcols - clbc) collb) $ \collb ->
            withArrayOrNull (map realToFrac   $ pad (numcols - cubc) colub) $ \colub ->
            withArrayOrNull (map realToFrac   $ pad (numcols - objc) obj  ) $ \obj   ->
            withArrayOrNull (map realToFrac   $ pad (numrows - rlbc) rowlb) $ \rowlb ->
            withArrayOrNull (map realToFrac   $ pad (numrows - rubc) rowub) $ \rowub ->
                Foreign.loadProblem model (fromIntegral numcols) (fromIntegral numrows) start index value collb colub obj rowlb rowub

    getObjSense :: s -> IO OptimizationDirection
    getObjSense model = toEnum <$> truncate <$> (1.0 +) <$> Foreign.getObjSense model

    setObjSense model dir = Foreign.setObjSense model $ (fromIntegral $ fromEnum dir) - 1.0
    setObjSense :: s -> OptimizationDirection -> IO ()

    rowBounds :: s -> IO [(Double, Double)]
    rowBounds model = do
        nr <- fromIntegral <$> Foreign.getNumRows model
        rl <- map realToFrac <$> (peekArray nr =<< Foreign.getRowLower model)
        ru <- map realToFrac <$> (peekArray nr =<< Foreign.getRowUpper model)
        return $ zip rl ru

    columnBounds :: s -> IO [(Double, Double, Double)]
    columnBounds model = do
        nc <- fromIntegral <$> Foreign.getNumCols model
        cl <- map realToFrac <$> (peekArray nc =<< Foreign.getColLower model)
        cu <- map realToFrac <$> (peekArray nc =<< Foreign.getColUpper model)
        ob <- map realToFrac <$> (peekArray nc =<< Foreign.getObjCoefficients model)
        return $ zip3 cl cu ob

    getElements :: s -> IO [LinearFunction]
    getElements model = do
        ne <- fromIntegral <$> Foreign.getNumElements model
        is <- map fromIntegral <$> (peekArray ne =<< Foreign.getIndices model)
        es <- map realToFrac <$> (peekArray ne =<< Foreign.getElements model)
        nc <- fromIntegral <$> Foreign.getNumCols model
        vs <- map fromIntegral <$> (peekArray nc =<< Foreign.getVectorStarts model)
        vl <- map fromIntegral <$> (peekArray nc =<< Foreign.getVectorLengths model)
        return $ map sparse $ segment 0 (zip vs vl) (zip is es)
        where segment :: Int -> [(Int, Int)] -> [a] -> [[a]]
              segment _ [] _ = [] -- There may be extraneous trailing elements.
              segment i ((j, _):_) _ | j < i = error "column start overshot"
              segment i ((j, n):sls) as = a:segment (j + n) sls as'
                  where (a, as') = splitAt n $ drop (j - i) as

    getObjValue :: s -> IO Double
    getObjValue = (fmap realToFrac) . Foreign.getObjValue

    solve :: s -> IO Status
    solve model = fmap (toEnum . (3 +) . fromIntegral) $ Foreign.initialSolve model

    getNumRows :: s -> IO Int
    getNumRows = (fmap fromIntegral) . Foreign.getNumRows

    getNumCols :: s -> IO Int
    getNumCols = (fmap fromIntegral) . Foreign.getNumCols

    getRowActivity :: s -> IO LinearFunction
    getRowActivity model = do
        nr <- fromIntegral <$> Foreign.getNumRows model
        dense <$> map realToFrac <$> (peekArray nr =<< Foreign.getRowActivity model)

    getColSolution :: s -> IO LinearFunction
    getColSolution model = do
        nc <- fromIntegral <$> Foreign.getNumCols model
        dense <$> map realToFrac <$> (peekArray nc =<< Foreign.getColSolution model)
