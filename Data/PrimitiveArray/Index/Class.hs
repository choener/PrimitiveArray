
module Data.PrimitiveArray.Index.Class where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Control.Monad (liftM2)
import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Test.QuickCheck



infixl 3 :.

-- | Strict pairs -- as in @repa@.

data a :. b = !a :. !b
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "StrictPair"
  [t| forall a b . (Unbox a, Unbox b) => (a:.b) -> (a,b) |]
  [| \(a:.b) -> (a, b) |]
  [| \(a,b)  -> (a:.b) |]

instance (Binary    a, Binary    b) => Binary    (a:.b)
instance (Serialize a, Serialize b) => Serialize (a:.b)
instance (ToJSON    a, ToJSON    b) => ToJSON    (a:.b)
instance (FromJSON  a, FromJSON  b) => FromJSON  (a:.b)

deriving instance (Read a, Read b) => Read (a:.b)

instance (NFData a, NFData b) => NFData (a:.b) where
  rnf (a:.b) = rnf a `seq` rnf b
  {-# Inline rnf #-}

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :. b) where
  arbitrary     = liftM2 (:.) arbitrary arbitrary
  shrink (a:.b) = [ (a':.b) | a' <- shrink a ] ++ [ (a:.b') | b' <- shrink b ]



infixl 3 :>

-- | A different version of strict pairs. Makes for simpler type inference in
-- multi-tape grammars.

data a :> b = !a :> !b
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "StrictIxPair"
  [t| forall a b . (Unbox a, Unbox b) => (a:>b) -> (a,b) |]
  [| \(a:>b) -> (a, b) |]
  [| \(a,b)  -> (a:>b) |]

instance (Binary    a, Binary    b) => Binary    (a:>b)
instance (Serialize a, Serialize b) => Serialize (a:>b)
instance (ToJSON    a, ToJSON    b) => ToJSON    (a:>b)
instance (FromJSON  a, FromJSON  b) => FromJSON  (a:>b)

deriving instance (Read a, Read b) => Read (a:>b)

instance (NFData a, NFData b) => NFData (a:>b) where
  rnf (a:>b) = rnf a `seq` rnf b
  {-# Inline rnf #-}

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :> b) where
  arbitrary = (:>) <$> arbitrary <*> arbitrary
  shrink (a:>b) = (:>) <$> shrink a <*> shrink b



-- | Base data constructor for multi-dimensional indices.

data Z = Z
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "Z"
  [t| Z -> () |]
  [| const () |]
  [| const Z  |]

instance Binary    Z
instance Serialize Z
instance ToJSON    Z
instance FromJSON  Z

instance Arbitrary Z where
  arbitrary = return Z

instance NFData Z where
  rnf Z = ()
  {-# Inline rnf #-}



-- | Index structures for complex, heterogeneous indexing. Mostly designed for
-- indexing in DP grammars, where the indices work for linear and context-free
-- grammars on one or more tapes, for strings, sets, later on tree structures.

class Index i where

  -- | Given a minimal size, a maximal size, and a current index, calculate
  -- the linear index.

  linearIndex :: i -> i -> i -> Int

  -- | Given an index element from the smallest subset, calculate the
  -- highest linear index that is *not* stored.

  smallestLinearIndex :: i -> Int -- LH i

  -- | Given an index element from the largest subset, calculate the
  -- highest linear index that *is* stored.

  largestLinearIndex :: i -> Int -- LH i

  -- | Given smallest and largest index, return the number of cells
  -- required for storage.

  size :: i -> i -> Int

  -- | Check if an index is within the bounds.

  inBounds :: i -> i -> i -> Bool

-- | Generate a stream of indices in correct order for dynamic programming.
-- Since the stream generators require @concatMap@ / @flatten@ we have to
-- write more specialized code for @(z:.IX)@ stuff.

class IndexStream i where

  -- | This generates an index stream suitable for @forward@ structure filling.
  -- The first index is the smallest (or the first indices considered are all
  -- equally small in partially ordered sets). Larger indices follow up until
  -- the largest one.

  streamUp   :: Monad m => i -> i -> Stream m i
  default streamUp :: (Monad m, IndexStream (Z:.i)) => i -> i -> Stream m i
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}

  -- | If 'streamUp' generates indices from smallest to largest, then
  -- 'streamDown' generates indices from largest to smallest. Outside grammars
  -- make implicit use of this. Asking for an axiom in backtracking requests
  -- the first element from this stream.

  streamDown :: Monad m => i -> i -> Stream m i
  default streamDown :: (Monad m, IndexStream (Z:.i)) => i -> i -> Stream m i
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}



instance Index Z where
  linearIndex _ _ _ = 0
  {-# INLINE linearIndex #-}
  smallestLinearIndex _ = 0
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex _ = 0
  {-# INLINE largestLinearIndex #-}
  size _ _ = 1
  {-# INLINE size #-}
  inBounds _ _ _ = True
  {-# INLINE inBounds #-}

instance IndexStream Z where
  streamUp   Z Z = SM.singleton Z
  {-# INLINE streamUp #-}
  streamDown Z Z = SM.singleton Z
  {-# INLINE streamDown #-}

instance (Index zs, Index z) => Index (zs:.z) where
  linearIndex (ls:.l) (hs:.h) (zs:.z) = linearIndex ls hs zs * (largestLinearIndex h + 1) + linearIndex l h z
  {-# INLINE linearIndex #-}
  smallestLinearIndex (ls:.l) = smallestLinearIndex ls * smallestLinearIndex l
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (hs:.h) = largestLinearIndex hs * largestLinearIndex h
  {-# INLINE largestLinearIndex #-}
  size (ls:.l) (hs:.h) = size ls hs * (size l h)
  {-# INLINE size #-}
  inBounds (ls:.l) (hs:.h) (zs:.z) = inBounds ls hs zs && inBounds l h z
  {-# INLINE inBounds #-}

