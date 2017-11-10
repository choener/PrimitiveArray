
-- | Index structure for context-free grammars on strings. A @Subword@ captures
-- a pair @(i,j)@ with @i<=j@.

module Data.PrimitiveArray.Index.Subword where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.Monad (filterM, guard)
import Data.Aeson (FromJSON,FromJSONKey,ToJSON,ToJSONKey)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Vector.Fusion.Stream.Monadic (Step(..), map)
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import Prelude hiding (map)
import Test.QuickCheck (Arbitrary(..), choose)
import Test.SmallCheck.Series as TS

import Math.TriangularNumbers

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Vector.Compat



-- | A subword wraps a pair of @Int@ indices @i,j@ with @i<=j@.
--
-- Subwords always yield the upper-triangular part of a rect-angular array.
-- This gives the quite curious effect that @(0,N)@ points to the
-- ``largest'' index, while @(0,0) ... (1,1) ... (k,k) ... (N,N)@ point to
-- the smallest. We do, however, use (0,0) as the smallest as (0,k) gives
-- successively smaller upper triangular parts.

newtype Subword t = Subword {fromSubword :: (Int:.Int)}
  deriving (Eq,Ord,Show,Generic,Read)

fromSubwordFst :: Subword t -> Int
fromSubwordFst (Subword (i:._)) = i
{-# Inline fromSubwordFst #-}

fromSubwordSnd :: Subword t -> Int
fromSubwordSnd (Subword (_:.j)) = j
{-# Inline fromSubwordSnd #-}

derivingUnbox "Subword"
  [t| forall t . Subword t -> (Int,Int) |]
  [| \ (Subword (i:.j)) -> (i,j) |]
  [| \ (i,j) -> Subword (i:.j) |]

instance Binary       (Subword t)
instance Serialize    (Subword t)
instance FromJSON     (Subword t)
instance FromJSONKey  (Subword t)
instance ToJSON       (Subword t)
instance ToJSONKey    (Subword t)
instance Hashable     (Subword t)

instance NFData (Subword t) where
  rnf (Subword (i:.j)) = i `seq` rnf j
  {-# Inline rnf #-}

-- | Create a @Subword t@ where @t@ is inferred.

subword :: Int -> Int -> Subword t
subword i j = Subword (i:.j)
{-# INLINE subword #-}

subwordI :: Int -> Int -> Subword I
subwordI i j = Subword (i:.j)
{-# INLINE subwordI #-}

subwordO :: Int -> Int -> Subword O
subwordO i j = Subword (i:.j)
{-# INLINE subwordO #-}

subwordC :: Int -> Int -> Subword C
subwordC i j = Subword (i:.j)
{-# INLINE subwordC #-}



instance Index (Subword t) where
  type LimitType (Subword t) = Int
  linearIndex n (Subword (i:.j)) = toLinear n (i,j)
  {-# Inline linearIndex #-}
  size _ n = linearizeUppertri (0,n)
  {-# Inline size #-}
  inBounds h (Subword (i:.j)) = 0<=i && i<=j && j<=h
  {-# Inline inBounds #-}

-- | @Subword I@ (inside)

instance IndexStream z => IndexStream (z:.Subword I) where
  streamUp   (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten (streamUpMk     h) (streamUpStep   l h) $ streamUp   ls hs
  streamDown (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten (streamDownMk l h) (streamDownStep   h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

-- | @Subword O@ (outside).
--
-- Note: @streamUp@ really needs to use @streamDownMk@ / @streamDownStep@
-- for the right order of indices!

instance IndexStream z => IndexStream (z:.Subword O) where
  streamUp   (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten (streamDownMk l h) (streamDownStep   h) $ streamUp   ls hs
  streamDown (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten (streamUpMk     h) (streamUpStep   l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

-- | @Subword C@ (complement)

instance IndexStream z => IndexStream (z:.Subword C) where
  streamUp   (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten (streamUpMk     h) (streamUpStep   l h) $ streamUp   ls hs
  streamDown (ls:.Subword (l:._)) (hs:.Subword (_:.h)) = flatten (streamDownMk l h) (streamDownStep   h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

-- | generic @mk@ for @streamUp@ / @streamDown@

streamUpMk h z = return (z,h,h)
{-# Inline [0] streamUpMk #-}

streamUpStep l h (z,i,j)
  | i < l     = return $ Done
  | j > h     = return $ Skip (z,i-1,i-1)
  | otherwise = return $ Yield (z:.subword i j) (z,i,j+1)
{-# Inline [0] streamUpStep #-}

streamDownMk l h z = return (z,l,h)
{-# Inline [0] streamDownMk #-}

streamDownStep h (z,i,j)
  | i > h     = return $ Done
  | j < i     = return $ Skip (z,i+1,h)
  | otherwise = return $ Yield (z:.subword i j) (z,i,j-1)
{-# Inline [0] streamDownStep #-}

instance (IndexStream (Z:.Subword t)) => IndexStream (Subword t) where
  streamUp l h = map (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}
  streamDown l h = map (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}

instance Arbitrary (Subword t) where
  arbitrary = do
    a <- choose (0,20)
    b <- choose (0,20)
    return $ Subword (min a b :. max a b)
  shrink (Subword (i:.j))
    | i<j       = [Subword (i:.j-1), Subword (i+1:.j)]
    | otherwise = []

instance Monad m => Serial m (Subword t) where
  series = do
    i <- TS.getNonNegative <$> series
    j <- TS.getNonNegative <$> series
    guard $ i<=j
    return $ subword i j
    {-
    let nns :: Series m Int = TS.getNonNegative <$> series
    ps <- nns >< nns
    let qs = [ subword i j | (i,j) <- ps, i<=j ]
    return qs
    -}

