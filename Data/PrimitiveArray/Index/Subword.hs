
-- | Index structure for context-free grammars on strings. A @Subword@ captures
-- a pair @(i,j)@ with @i<=j@.

module Data.PrimitiveArray.Index.Subword where

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Serialize (Serialize)
import Data.Vector.Fusion.Stream.Monadic (Step(..), flatten, map)
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(..), choose)
import Prelude hiding (map)

import Data.PrimitiveArray.Index.Class



-- | A subword wraps a pair of @Int@ indices @i,j@ with @i<=j@.
--
-- Subwords always yield the upper-triangular part of a rect-angular array.
-- This gives the quite curious effect that @(0,N)@ points to the
-- ``largest'' index, while @(0,0) ... (1,1) ... (k,k) ... (N,N)@ point to
-- the smallest. We do, however, use (0,0) as the smallest as (0,k) gives
-- successively smaller upper triangular parts.

newtype Subword = Subword {fromSubword :: (Int:.Int)}
  deriving (Eq,Ord,Show,Generic,Read)

derivingUnbox "Subword"
  [t| Subword -> (Int,Int) |]
  [| \ (Subword (i:.j)) -> (i,j) |]
  [| \ (i,j) -> Subword (i:.j) |]

instance Binary    Subword
instance Serialize Subword
instance FromJSON  Subword
instance ToJSON    Subword

instance NFData Subword where
  rnf (Subword (i:.j)) = i `seq` rnf j
  {-# Inline rnf #-}

subword :: Int -> Int -> Subword
subword i j = Subword (i:.j)
{-# INLINE subword #-}

-- | triangular numbers
--
-- A000217

triangularNumber :: Int -> Int
triangularNumber x = (x * (x+1)) `quot` 2
{-# INLINE triangularNumber #-}

-- | Size of an upper triangle starting at 'i' and ending at 'j'. "(0,N)" what
-- be the normal thing to use.

upperTri :: Subword -> Int
upperTri (Subword (i:.j)) = triangularNumber $ j-i+1
{-# INLINE upperTri #-}

-- | Subword indexing. Given the longest subword and the current subword,
-- calculate a linear index "[0,..]". "(l,n)" in this case means "l"ower bound,
-- length "n". And "(i,j)" is the normal index.
--
-- TODO probably doesn't work right with non-zero base ?!

subwordIndex :: Subword -> Subword -> Int
subwordIndex (Subword (l:.n)) (Subword (i:.j)) = adr n (i,j) -- - adr n (l,n)
  where
    adr n (i,j) = (n+1)*i - triangularNumber i + j
{-# INLINE subwordIndex #-}

subwordFromIndex :: Subword -> Int -> Subword
subwordFromIndex = error "subwordFromIndex not implemented"
{-# INLINE subwordFromIndex #-}



instance Index Subword where
  linearIndex _ h i = subwordIndex h i
  {-# Inline linearIndex #-}
  smallestLinearIndex _ = error "still needed?"
  {-# Inline smallestLinearIndex #-}
  largestLinearIndex = upperTri
  {-# Inline largestLinearIndex #-}
  size _ h = upperTri h
  {-# Inline size #-}
  inBounds _ (Subword (_:.h)) (Subword (i:.j)) = 0<=i && i<=j && j<=h
  {-# Inline inBounds #-}

instance IndexStream z => IndexStream (z:.Subword) where
  streamUp (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten mk step Unknown $ streamUp ls hs
    where mk z = return (z,h,h)
          step (z,i,j)
            | i < l     = return $ Done
            | j > h     = return $ Skip (z,i-1,i-1)
            | otherwise = return $ Yield (z:.subword i j) (z,i,j+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten mk step Unknown $ streamDown ls hs
    where mk z = return (z,l,h)
          step (z,i,j)
            | i > h     = return $ Done
            | j < i     = return $ Skip (z,i+1,h)
            | otherwise = return $ Yield (z:.subword i j) (z,i,j-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

-- Default methods don't inline in a good way!

instance IndexStream Subword where
  streamUp l h = map (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}
  streamDown l h = map (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}

instance Arbitrary Subword where
  arbitrary = do
    a <- choose (0,100)
    b <- choose (0,100)
    return $ Subword (min a b :. max a b)
  shrink (Subword (i:.j))
    | i<j       = [Subword (i:.j-1)]
    | otherwise = []

