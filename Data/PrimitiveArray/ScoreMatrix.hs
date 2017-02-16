
-- | Simple score and distance matrices. These are two-dimensional tables
-- together with row and column vectors of names.

module Data.PrimitiveArray.ScoreMatrix where

import           Control.Monad (when,unless)
import           Data.Text (Text)
import           Data.Vector.Unboxed (Unbox)
import           Numeric.Log
import qualified Data.Text as T
import qualified Data.Vector as V
import           System.Exit (exitFailure)

import           Data.PrimitiveArray hiding (map)
import qualified Data.PrimitiveArray as PA



-- | NxN sized score matrices
--
-- TODO needs a vector with the column names!

data ScoreMatrix t = ScoreMatrix
  { scoreMatrix :: !(Unboxed (Z:.Int:.Int) t)
  , rowNames    :: !(V.Vector Text)
  , colNames    :: !(V.Vector Text)
  } deriving (Eq,Show)

-- | Get the distance between edges @(From,To)@.

(.!.) :: Unbox t => ScoreMatrix t -> (Int,Int) -> t
ScoreMatrix mat _ _ .!. (f,t) = mat ! (Z:.f:.t)
{-# Inline (.!.) #-}

-- | Get the name of the node at an row index

rowNameOf :: ScoreMatrix t -> Int -> Text
rowNameOf (ScoreMatrix _ rs cs) k = rs V.! k
{-# Inline rowNameOf #-}

-- | Get the name of the node at an column index

colNameOf :: ScoreMatrix t -> Int -> Text
colNameOf (ScoreMatrix _ rs cs) k = cs V.! k
{-# Inline colNameOf #-}

-- | Number of rows in a score matrix.

numRows :: Unbox t => ScoreMatrix t -> Int
numRows (ScoreMatrix mat _ _) = let ((Z:.0:.0),(Z:.n':._)) = bounds mat in n' + 1
{-# Inline numRows #-}

-- | Number of columns in a score matrix.

numCols :: Unbox t => ScoreMatrix t -> Int
numCols (ScoreMatrix mat _ _) = let ((Z:.0:.0),(Z:._:.n')) = bounds mat in n' + 1
{-# Inline numCols #-}

listOfRowNames :: ScoreMatrix t -> [Text]
listOfRowNames (ScoreMatrix _ ns _) = V.toList ns

listOfColNames :: ScoreMatrix t -> [Text]
listOfColNames (ScoreMatrix _ _ ns) = V.toList ns

-- | Turns a @ScoreMatrix@ for distances into one scaled by "temperature" for
-- Inside/Outside calculations. Each value is scaled by
-- @\k -> exp $ negate k / r * t@ where
-- r = (n-1) * d
-- d = mean of genetic distance
--
-- TODO Again, there is overlap and we should really have @newtype
-- Distance@ and friends.
--
-- TODO @newtype Temperature = Temperature Double@
--
-- TODO fix for rows /= cols!!!

toPartMatrix
  :: Double
  -- ^ temperature
  -> ScoreMatrix Double
  -> ScoreMatrix (Log Double)
toPartMatrix t scoreMat@(ScoreMatrix mat rns cns) = ScoreMatrix p rns cns
  where p = PA.map (\k -> Exp {- . log . exp -} $ negate k / (r * t)) mat
        n = numRows scoreMat
        d = Prelude.sum [ mat ! (Z:.i:.j) | i <- [0..n-1], j <- [i+1..n-1] ] / fromIntegral (n*(n-1))
        r = fromIntegral (n-1) * d

-- | Import data.
--
-- TODO Should be generalized because @Lib-BiobaseBlast@ does almost the
-- same thing.

fromFile :: FilePath -> IO (ScoreMatrix Double)
fromFile fp = do
  ls <- lines <$> readFile fp
  when (null ls) $ do
    putStrLn $ fp ++ " is empty"
    exitFailure
  let mat' = map (map read . tail . words) $ tail ls
  let n = length mat'
  unless (all ((==n) . length) mat') $ do
    putStrLn $ fp ++ " is not a NxN matrix"
    print mat'
    exitFailure
  let mat = PA.fromAssocs (Z:.0:.0) (Z:.n-1:.n-1) 0
          $ concatMap (\(r,es) -> [ ((Z:.r:.c),e) | (c,e) <- zip [0..] es ])
          $ zip [0..] mat' -- rows
  let rowNames = V.fromList . map T.pack . drop 1 . words $ head ls
  let colNames = V.fromList . map (T.pack . head . words) $ tail ls
  return $ ScoreMatrix mat rowNames colNames

