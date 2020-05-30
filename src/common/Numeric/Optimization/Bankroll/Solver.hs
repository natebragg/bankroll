module Numeric.Optimization.Bankroll.Solver (
    Foreign.newModel,
    Foreign.isAbandoned,
    Foreign.isProvenOptimal,

    Solver(..),
    OptimizationDirection(..),
    Status(..),
) where

import Foreign.C.String (withCString)
import Foreign.Marshal.Array (peekArray)
import Numeric.Optimization.Bankroll.LinearFunction (unpack)
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

    getElements :: s -> IO [[Double]]
    getElements model = do
        ne <- fromIntegral <$> Foreign.getNumElements model
        is <- map fromIntegral <$> (peekArray ne =<< Foreign.getIndices model)
        es <- map realToFrac <$> (peekArray ne =<< Foreign.getElements model)
        nc <- fromIntegral <$> Foreign.getNumCols model
        vl <- map fromIntegral <$> (peekArray nc =<< Foreign.getVectorLengths model)
        return $ map unpack $ segment vl (zip is es)
        where segment :: [Int] -> [a] -> [[a]]
              segment [] [] = []
              segment [] as = [as]
              segment (n:ls) as = a:segment ls as'
                  where (a, as') = splitAt n as

    getObjValue :: s -> IO Double
    getObjValue = (fmap realToFrac) . Foreign.getObjValue

    solve :: s -> IO Status
    solve model = fmap (toEnum . (3 +) . fromIntegral) $ Foreign.initialSolve model

    getNumRows :: s -> IO Int
    getNumRows = (fmap fromIntegral) . Foreign.getNumRows

    getNumCols :: s -> IO Int
    getNumCols = (fmap fromIntegral) . Foreign.getNumCols

    getRowActivity :: s -> IO [Double]
    getRowActivity model = do
        nr <- fromIntegral <$> Foreign.getNumRows model
        map realToFrac <$> (peekArray nr =<< Foreign.getRowActivity model)

    getColSolution :: s -> IO [Double]
    getColSolution model = do
        nc <- fromIntegral <$> Foreign.getNumCols model
        map realToFrac <$> (peekArray nc =<< Foreign.getColSolution model)
