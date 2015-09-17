
-- | @Point@ index structures are used for left- and right-linear grammars.
-- Such grammars have at most one syntactic symbol on each r.h.s. of a rule.
-- The syntactic symbol needs to be in an outermost position.

module Data.PrimitiveArray.Index.Point where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Data.Aeson
import           Data.Binary
import           Data.Bits
import           Data.Bits.Extras (Ranked)
import           Data.Hashable (Hashable)
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics (Generic)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck

import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC
import           Data.PrimitiveArray.Vector.Compat



-- | A point in a left-linear grammar. The syntactic symbol is in left-most
-- position.

newtype PointL t = PointL {fromPointL :: Int}
  deriving (Eq,Read,Show,Generic)

-- | A point in a right-linear grammars.

newtype PointR t = PointR {fromPointR :: Int}
  deriving (Eq,Read,Show,Generic)



derivingUnbox "PointL"
  [t| forall t . PointL t -> Int    |]
  [| \ (PointL i) -> i |]
  [| \ i -> PointL i   |]

instance Binary    (PointL t)
instance Serialize (PointL t)
instance FromJSON  (PointL t)
instance ToJSON    (PointL t)
instance Hashable  (PointL t)

instance NFData (PointL t) where
  rnf (PointL l) = rnf l
  {-# Inline rnf #-}

instance Index (PointL t) where
  linearIndex _ _ (PointL z) = z
  {-# INLINE linearIndex #-}
  smallestLinearIndex (PointL l) = error "still needed?"
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (PointL h) = h
  {-# INLINE largestLinearIndex #-}
  size (_) (PointL h) = h + 1
  {-# INLINE size #-}
  inBounds (_) (PointL h) (PointL x) = 0<=x && x<=h
  {-# INLINE inBounds #-}

instance IndexStream z => IndexStream (z:.PointL I) where
  streamUp   (ls:.PointL lf) (hs:.PointL ht) = flatten (streamUpMk   lf) (streamUpStep   ht) $ streamUp ls hs
  streamDown (ls:.PointL lf) (hs:.PointL ht) = flatten (streamDownMk ht) (streamDownStep lf) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.PointL O) where
  streamUp   (ls:.PointL lf) (hs:.PointL ht) = flatten (streamDownMk ht) (streamDownStep lf) $ streamUp   ls hs
  streamDown (ls:.PointL lf) (hs:.PointL ht) = flatten (streamUpMk   lf) (streamUpStep   ht) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.PointL C) where
  streamUp   (ls:.PointL lf) (hs:.PointL ht) = flatten (streamUpMk   lf) (streamUpStep   ht) $ streamUp ls hs
  streamDown (ls:.PointL lf) (hs:.PointL ht) = flatten (streamDownMk ht) (streamDownStep lf) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

streamUpMk lf z = return (z,lf)
{-# Inline [0] streamUpMk #-}

streamUpStep ht (z,k)
  | k > ht    = return $ SM.Done
  | otherwise = return $ SM.Yield (z:.PointL k) (z,k+1)
{-# Inline [0] streamUpStep #-}

streamDownMk ht z = return (z,ht)
{-# Inline [0] streamDownMk #-}

streamDownStep lf (z,k)
  | k < lf    = return $ SM.Done
  | otherwise = return $ SM.Yield (z:.PointL k) (z,k-1)
{-# Inline [0] streamDownStep #-}

{-
instance IndexStream z => IndexStream (z:.PointL) where
  streamUp (ls:.PointL lf) (hs:.PointL ht) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = return (z,lf)
          step (z,k)
            | k > ht    = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.PointL k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.PointL lf) (hs:.PointL ht) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = return (z,ht)
          step (z,k)
            | k < lf    = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.PointL k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}
-}

instance IndexStream (Z:.PointL t) => IndexStream (PointL t)

instance Arbitrary (PointL t) where
  arbitrary = do
    b <- choose (0,100)
    return $ PointL b
  shrink (PointL j)
    | 0<j = [PointL $ j-1]
    | otherwise = []



-- * @PointR@
--
-- TODO complete instances

derivingUnbox "PointR"
  [t| forall t . PointR t -> Int    |]
  [| \ (PointR i) -> i |]
  [| \ i -> PointR i   |]

instance Binary    (PointR t)
instance Serialize (PointR t)
instance FromJSON  (PointR t)
instance ToJSON    (PointR t)
instance Hashable  (PointR t)

instance NFData (PointR t) where
  rnf (PointR l) = rnf l
  {-# Inline rnf #-}

instance Index (PointR t) where
  linearIndex l _ (PointR z) = undefined
  {-# INLINE linearIndex #-}
  smallestLinearIndex = undefined
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex = undefined
  {-# INLINE largestLinearIndex #-}
  size = undefined
  {-# INLINE size #-}

