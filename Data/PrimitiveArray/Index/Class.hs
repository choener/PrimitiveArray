
module Data.PrimitiveArray.Index.Class where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Control.Monad (liftM2)
import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable)
import           Data.Proxy
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
instance (Hashable  a, Hashable  b) => Hashable  (a:.b)

instance (ToJSON a  , ToJSONKey   a, ToJSON b  , ToJSONKey   b) => ToJSONKey   (a:.b)
instance (FromJSON a, FromJSONKey a, FromJSON b, FromJSONKey b) => FromJSONKey (a:.b)

deriving instance (Read a, Read b) => Read (a:.b)

instance (NFData a, NFData b) => NFData (a:.b) where
  rnf (a:.b) = rnf a `seq` rnf b
  {-# Inline rnf #-}

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :. b) where
  arbitrary     = liftM2 (:.) arbitrary arbitrary
  shrink (a:.b) = [ (a':.b) | a' <- shrink a ] ++ [ (a:.b') | b' <- shrink b ]



infixr 3 :>

-- | A different version of strict pairs. Makes for simpler type inference in
-- multi-tape grammars. We use @:>@ when we have special needs, like
-- non-recursive instances on inductives tuples, as used for set indices.
--
-- This one is @infixr@ so that in @a :> b@ we can have the main type in
-- @a@ and the specializing types in @b@ and then dispatch on @a :> ts@
-- with @ts@ maybe a chain of @:>@.

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
instance (Hashable  a, Hashable  b) => Hashable  (a:>b)

deriving instance (Read a, Read b) => Read (a:>b)

instance (NFData a, NFData b) => NFData (a:>b) where
  rnf (a:>b) = rnf a `seq` rnf b
  {-# Inline rnf #-}

--instance (Arbitrary a, Arbitrary b) => Arbitrary (a :> b) where
--  arbitrary = (:>) <$> arbitrary <*> arbitrary
--  shrink (a:>b) = (:>) <$> shrink a <*> shrink b



-- | Base data constructor for multi-dimensional indices.

data Z = Z
  deriving (Eq,Ord,Read,Show,Generic)

derivingUnbox "Z"
  [t| Z -> () |]
  [| const () |]
  [| const Z  |]

instance Binary    Z
instance Serialize Z
instance ToJSON    Z
instance FromJSON  Z
instance Hashable  Z

instance Arbitrary Z where
  arbitrary = return Z

instance NFData Z where
  rnf Z = ()
  {-# Inline rnf #-}



-- | Index structures for complex, heterogeneous indexing. Mostly designed for
-- indexing in DP grammars, where the indices work for linear and context-free
-- grammars on one or more tapes, for strings, sets, later on tree structures.

class Index i where

  -- | Data structure encoding the upper limit for each array.
  type UpperLimit i ∷ *

  -- | Given a maximal size, and a current index, calculate
  -- the linear index.

  linearIndex ∷ UpperLimit i → i → Int

  -- | Given the 'UpperLimit', return the number of cells required for storage.
  --
  -- TODO should, in principle, only require @UpperLimit i@, not @i@, but then
  -- we need @data UpperLmit@

  size ∷ UpperLimit i → i → Int

  -- | Check if an index is within the bounds.

  inBounds ∷ UpperLimit i → i → Bool



-- | Generate a stream of indices in correct order for dynamic programming.
-- Since the stream generators require @concatMap@ / @flatten@ we have to
-- write more specialized code for @(z:.IX)@ stuff.
--
-- TODO variants that just take an 'UpperLimit' and stream the whole range.

class IndexStream i where
  -- | This generates an index stream suitable for @forward@ structure filling.
  -- The first index is the smallest (or the first indices considered are all
  -- equally small in partially ordered sets). Larger indices follow up until
  -- the largest one.
  streamUp ∷ Monad m ⇒ i → i → Stream m i
  -- | If 'streamUp' generates indices from smallest to largest, then
  -- 'streamDown' generates indices from largest to smallest. Outside grammars
  -- make implicit use of this. Asking for an axiom in backtracking requests
  -- the first element from this stream.
  streamDown ∷ Monad m ⇒ i → i → Stream m i



instance Index Z where
  type UpperLimit Z = Z
  linearIndex _ _ = 0
  {-# INLINE linearIndex #-}
  size _ _ = 1
  {-# INLINE size #-}
  inBounds _ _ = True
  {-# INLINE inBounds #-}

instance IndexStream Z where
  streamUp   Z Z = SM.singleton Z
  {-# INLINE streamUp #-}
  streamDown Z Z = SM.singleton Z
  {-# INLINE streamDown #-}

instance (Index zs, Index z) => Index (zs:.z) where
  type UpperLimit (zs:.z) = (UpperLimit zs:.UpperLimit z)
  linearIndex (hs:.h) (zs:.z) = linearIndex hs zs * (size h z) + linearIndex h z
  {-# INLINE linearIndex #-}
  size (ls:.l) (hs:.h) = size ls hs * (size l h)
  {-# INLINE size #-}
  inBounds (hs:.h) (zs:.z) = inBounds hs zs && inBounds h z
  {-# INLINE inBounds #-}

instance (Index zs, Index z) => Index (zs:>z) where
  type UpperLimit (zs:>z) = UpperLimit zs:>UpperLimit z
  linearIndex (hs:>h) (zs:>z) = linearIndex hs zs * (size h z) + linearIndex h z
  {-# INLINE linearIndex #-}
  size (ls:>l) (hs:>h) = size ls hs * (size l h)
  {-# INLINE size #-}
  inBounds (hs:>h) (zs:>z) = inBounds hs zs && inBounds h z
  {-# INLINE inBounds #-}

