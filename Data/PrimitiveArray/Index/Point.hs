
{-# Language MagicHash #-}

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
import           GHC.Exts
import           GHC.Generics (Generic)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck as TQ
import           Test.SmallCheck.Series as TS

import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC



-- | A point in a left-linear grammar. The syntactic symbol is in left-most
-- position.

newtype PointL t = PointL {fromPointL :: Int}
  deriving (Eq,Ord,Read,Show,Generic)

pointLI :: Int -> PointL I
pointLI = PointL
{-# Inline pointLI #-}

pointLO :: Int -> PointL O
pointLO = PointL
{-# Inline pointLO #-}

pointLC :: Int -> PointL C
pointLC = PointL
{-# Inline pointLC #-}

-- | A point in a right-linear grammars.

newtype PointR t = PointR {fromPointR :: Int}
  deriving (Eq,Ord,Read,Show,Generic)



derivingUnbox "PointL"
  [t| forall t . PointL t -> Int    |]
  [| \ (PointL i) -> i |]
  [| \ i -> PointL i   |]

instance Binary       (PointL t)
instance Serialize    (PointL t)
instance FromJSON     (PointL t)
instance FromJSONKey  (PointL t)
instance ToJSON       (PointL t)
instance ToJSONKey    (PointL t)
instance Hashable     (PointL t)

instance NFData (PointL t) where
  rnf (PointL l) = rnf l
  {-# Inline rnf #-}

instance Index (PointL t) where
  newtype LimitType (PointL t) = LtPointL Int
  linearIndex _ (PointL z) = z
  {-# INLINE linearIndex #-}
  size (LtPointL h) = h + 1
  {-# INLINE size #-}
  inBounds (LtPointL h) (PointL x) = 0<=x && x<=h
  {-# INLINE inBounds #-}
  zeroBound = PointL 0
  {-# Inline [0] zeroBound #-}
  zeroBound' = LtPointL 0
  {-# Inline [0] zeroBound' #-}
  sizeIsValid (LtPointL h) = h <= maxBound
  {-# Inline [0] sizeIsValid #-}

instance IndexStream z => IndexStream (z:.PointL I) where
  streamUp   (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamUpMk   lf) (streamUpStep   ht) $ streamUp ls hs
  streamDown (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamDownMk ht) (streamDownStep lf) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

instance IndexStream z => IndexStream (z:.PointL O) where
  streamUp   (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamDownMk ht) (streamDownStep lf) $ streamUp   ls hs
  streamDown (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamUpMk   lf) (streamUpStep   ht) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

instance IndexStream z => IndexStream (z:.PointL C) where
  streamUp   (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamUpMk   lf) (streamUpStep   ht) $ streamUp ls hs
  streamDown (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamDownMk ht) (streamDownStep lf) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

data SP z = SP !z !Int#

streamUpMk (I# lf) z = return $ SP z lf
{-# Inline [0] streamUpMk #-}

streamUpStep (I# ht) (SP z k)
  | 1# <- k ># ht = return $ SM.Done
  | otherwise     = return $ SM.Yield (z:.PointL (I# k)) (SP z (k +# 1#))
{-# Inline [0] streamUpStep #-}

streamDownMk (I# ht) z = return $ SP z ht
{-# Inline [0] streamDownMk #-}

streamDownStep (I# lf) (SP z k)
  | 1# <- k <# lf = return $ SM.Done
  | otherwise     = return $ SM.Yield (z:.PointL (I# k)) (SP z (k -# 1#))
{-# Inline [0] streamDownStep #-}

instance IndexStream (Z:.PointL t) => IndexStream (PointL t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}


instance Arbitrary (PointL t) where
  arbitrary = do
    b <- choose (0,100)
    return $ PointL b
  shrink (PointL j)
    | 0<j = [PointL $ j-1]
    | otherwise = []

instance Monad m => Serial m (PointL t) where
  series = PointL . TS.getNonNegative <$> series



-- -- * @PointR@
-- --
-- -- TODO complete instances
-- 
-- derivingUnbox "PointR"
--   [t| forall t . PointR t -> Int    |]
--   [| \ (PointR i) -> i |]
--   [| \ i -> PointR i   |]
-- 
-- instance Binary    (PointR t)
-- instance Serialize (PointR t)
-- instance FromJSON  (PointR t)
-- instance ToJSON    (PointR t)
-- instance Hashable  (PointR t)
-- 
-- instance NFData (PointR t) where
--   rnf (PointR l) = rnf l
--   {-# Inline rnf #-}
-- 
-- instance Index (PointR t) where
--   linearIndex l (PointR z) = undefined
--   {-# INLINE linearIndex #-}
--   size = undefined
--   {-# INLINE size #-}

