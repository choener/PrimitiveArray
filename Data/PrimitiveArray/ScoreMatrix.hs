
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
  , scoreNodes  :: !(Unboxed Int t)
  , rowNames    :: !(V.Vector Text)
  , colNames    :: !(V.Vector Text)
  } deriving (Eq,Show)

-- | Get the distance between edges @(From,To)@.

(.!.) :: Unbox t => ScoreMatrix t -> (Int,Int) -> t
ScoreMatrix mat _ _ _ .!. (f,t) = mat ! (Z:.f:.t)
{-# Inline (.!.) #-}

-- | If the initial node has a "distance", it'll be here

nodeDist :: Unbox t => ScoreMatrix t -> Int -> t
nodeDist ScoreMatrix{..} k = scoreNodes ! k

-- | Get the name of the node at an row index

rowNameOf :: ScoreMatrix t -> Int -> Text
rowNameOf ScoreMatrix{..} k = rowNames V.! k
{-# Inline rowNameOf #-}

-- | Get the name of the node at an column index

colNameOf :: ScoreMatrix t -> Int -> Text
colNameOf ScoreMatrix{..} k = colNames V.! k
{-# Inline colNameOf #-}

-- | Number of rows in a score matrix.

numRows :: Unbox t => ScoreMatrix t -> Int
numRows ScoreMatrix{..} = let ((Z:.0:.0),(Z:.n':._)) = bounds scoreMatrix in n' + 1
{-# Inline numRows #-}

-- | Number of columns in a score matrix.

numCols :: Unbox t => ScoreMatrix t -> Int
numCols ScoreMatrix{..} = let ((Z:.0:.0),(Z:._:.n')) = bounds scoreMatrix in n' + 1
{-# Inline numCols #-}

listOfRowNames :: ScoreMatrix t -> [Text]
listOfRowNames ScoreMatrix{..} = V.toList rowNames

listOfColNames :: ScoreMatrix t -> [Text]
listOfColNames ScoreMatrix{..} = V.toList colNames

-- | Turns a @ScoreMatrix@ for distances into one scaled by "temperature" for
-- Inside/Outside calculations. Each value is scaled by
-- @\k -> exp $ negate k / r * t@ where
-- r = (n-1) * d
-- d = mean of genetic distance
--
-- Node scores are turned directly into probabilities.
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
toPartMatrix t scoreMat@(ScoreMatrix mat sn rns cns) = ScoreMatrix p psn rns cns
  where p = PA.map (\k -> Exp {- . log . exp -} $ negate k / (r * t)) mat
        psn = PA.map (\k -> Exp $ negate k) sn
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
  let scoreMatrix = PA.fromAssocs (Z:.0:.0) (Z:.n-1:.n-1) 0
          $ concatMap (\(r,es) -> [ ((Z:.r:.c),e) | (c,e) <- zip [0..] es ])
          $ zip [0..] mat' -- rows
  let scoreNodes = PA.fromAssocs 0 (n-1) 0 []
  let rowNames = V.fromList . map T.pack . drop 1 . words $ head ls
  let colNames = V.fromList . map (T.pack . head . words) $ tail ls
  return $ ScoreMatrix{..} -- mat rowNames colNames (V.fromList $ replicate n 0)

