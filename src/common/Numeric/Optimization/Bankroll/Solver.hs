{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Numeric.Optimization.Bankroll.Solver (
    Foreign.newModel,
    Foreign.isAbandoned,
    Foreign.isProvenOptimal,

    Solver(..),
    model, setModel,
    evalSolverIO,
    evalSolver,
    doSolverIO,
    doSolver,
    MonadSolver(..),
    OptimizationDirection(..),
    Status(..),
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.Foldable (toList)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray, newArray)
import Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    dense,
    sparse,
    coefficientOffsets,
    transpose,
    )
import qualified Numeric.Optimization.Bankroll.Solver.Foreign as Foreign
import System.IO.Unsafe (unsafePerformIO)

data OptimizationDirection = Maximize | Ignore | Minimize
    deriving (Eq, Ord, Enum, Show)

data Status = Finished
            | Stopped
            | Errors
            | UserStopped
            | Unknown
    deriving (Eq, Ord, Enum, Show)

newtype Solver s a = Solver { unSolver :: StateT s IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

evalSolverIO :: MonadSolver (Solver s) => Solver s a -> s -> IO a
evalSolverIO = evalStateT . unSolver

evalSolver :: MonadSolver (Solver s) => Solver s a -> s -> a
evalSolver = (unsafePerformIO .) . evalSolverIO

doSolverIO :: MonadSolver (Solver s) => Solver s a -> IO a
doSolverIO m = evalSolverIO (Foreign.newModel >> m) undefined

doSolver :: MonadSolver (Solver s) => Solver s a -> a
doSolver m = evalSolver (Foreign.newModel >> m) undefined

model :: Solver s s
model = get

setModel :: s -> Solver s ()
setModel = put

class (MonadIO m, Foreign.MonadSolver m) => MonadSolver m where
    readMps :: String -> Bool -> Bool -> m Int
    readMps fn keepNames ignoreErrors = do
        fn <- liftIO $ newCString fn
        r <- fromIntegral <$> Foreign.readMps fn keepNames ignoreErrors
        liftIO $ free fn
        return r

    loadProblem :: [LinearFunction] -> LinearFunction -> LinearFunction -> LinearFunction -> LinearFunction -> LinearFunction -> m ()
    loadProblem values collb colub obj rowlb rowub = do
        let (start, index, value) = coefficientOffsets $ transpose values -- loadProblem expects columns, but values is a list of rows
            [clbc, cubc, objc, rlbc, rubc] = map length [collb, colub, obj, rowlb, rowub]
            numcols = maximum $ clbc:cubc:objc:map length values
            numrows = maximum $ [rlbc, rubc, length values]
            pad n f = if null f then [] else toList f ++ replicate n 0.0
            newArrayOrNull xs = if null xs then pure nullPtr else liftIO $ newArray xs
        start <- newArrayOrNull (map fromIntegral                      start)
        index <- newArrayOrNull (map fromIntegral                      index)
        value <- newArrayOrNull (map realToFrac                        value)
        collb <- newArrayOrNull (map realToFrac $ pad (numcols - clbc) collb)
        colub <- newArrayOrNull (map realToFrac $ pad (numcols - cubc) colub)
        obj   <- newArrayOrNull (map realToFrac $ pad (numcols - objc) obj  )
        rowlb <- newArrayOrNull (map realToFrac $ pad (numrows - rlbc) rowlb)
        rowub <- newArrayOrNull (map realToFrac $ pad (numrows - rubc) rowub)
        Foreign.loadProblem (fromIntegral numcols) (fromIntegral numrows) start index value collb colub obj rowlb rowub
        liftIO $ do free start; free index; free value; free collb; free colub; free obj; free rowlb; free rowub

    getObjSense :: m OptimizationDirection
    getObjSense = toEnum <$> truncate <$> (1.0 +) <$> Foreign.getObjSense

    setObjSense :: OptimizationDirection -> m ()
    setObjSense dir = Foreign.setObjSense $ (fromIntegral $ fromEnum dir) - 1.0

    rowBounds :: m [(Double, Double)]
    rowBounds = do
        nr <- getNumRows
        rl <- map realToFrac <$> (liftIO . peekArray nr =<< Foreign.getRowLower)
        ru <- map realToFrac <$> (liftIO . peekArray nr =<< Foreign.getRowUpper)
        return $ zip rl ru

    columnBounds :: m [(Double, Double, Double)]
    columnBounds = do
        nc <- getNumCols
        cl <- map realToFrac <$> (liftIO . peekArray nc =<< Foreign.getColLower)
        cu <- map realToFrac <$> (liftIO . peekArray nc =<< Foreign.getColUpper)
        ob <- map realToFrac <$> (liftIO . peekArray nc =<< Foreign.getObjCoefficients)
        return $ zip3 cl cu ob

    getElements :: m [LinearFunction]
    getElements = do
        ne <- fromIntegral <$> Foreign.getNumElements
        is <- map fromIntegral <$> (liftIO . peekArray ne =<< Foreign.getIndices)
        es <- map realToFrac   <$> (liftIO . peekArray ne =<< Foreign.getElements)
        nc <- getNumCols
        vs <- map fromIntegral <$> (liftIO . peekArray nc =<< Foreign.getVectorStarts)
        vl <- map fromIntegral <$> (liftIO . peekArray nc =<< Foreign.getVectorLengths)
        return $ map sparse $ segment 0 (zip vs vl) (zip is es)
        where segment :: Int -> [(Int, Int)] -> [a] -> [[a]]
              segment _ [] _ = [] -- There may be extraneous trailing elements.
              segment i ((j, _):_) _ | j < i = error "column start overshot"
              segment i ((j, n):sls) as = a:segment (j + n) sls as'
                  where (a, as') = splitAt n $ drop (j - i) as

    getObjValue :: m Double
    getObjValue = realToFrac <$> Foreign.getObjValue

    solve :: m Status

    getNumRows :: m Int
    getNumRows = fromIntegral <$> Foreign.getNumRows

    getNumCols :: m Int
    getNumCols = fromIntegral <$> Foreign.getNumCols

    getRowActivity :: m LinearFunction
    getRowActivity = do
        nr <- getNumRows
        dense <$> map realToFrac <$> (liftIO . peekArray nr =<< Foreign.getRowActivity)

    getColSolution :: m LinearFunction
    getColSolution = do
        nc <- getNumCols
        dense <$> map realToFrac <$> (liftIO . peekArray nc =<< Foreign.getColSolution)
