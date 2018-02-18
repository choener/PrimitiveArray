
module Data.PrimitiveArray.Index.Class where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Control.Monad (liftM2)
import           Control.Monad.Except
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
import           Text.Printf



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
  data LimitType i ∷ *
  -- | Given a maximal size, and a current index, calculate
  -- the linear index.
  linearIndex ∷ LimitType i → i → Int
  -- | Given the 'LimitType', return the number of cells required for storage.
  size ∷ LimitType i → Int
  -- | Check if an index is within the bounds.
  inBounds ∷ LimitType i → i → Bool
  -- | A lower bound of @zero@
  zeroBound ∷ i
  -- | A lower bound of @zero@ but for a @LimitType i@.
  zeroBound' ∷ LimitType i
  -- | The list of cell sizes for each dimension. its product yields the total
  -- size.
  totalSize ∷ LimitType i → [Integer]

-- | Given the maximal number of cells (@Word@, because this is the pointer
-- limit for the machine), and the list of sizes, will check if this is still
-- legal. Consider dividing the @Word@ by the actual memory requirements for
-- each cell, to get better exception handling for too large arrays.
--
-- One list should be given for each array.

sizeIsValid ∷ Monad m ⇒ Word → [[Integer]] → ExceptT SizeError m CellSize
sizeIsValid maxCells cells = do
  let ps = map product cells
      s  = sum ps
  unless (fromIntegral maxCells <= s) $
    throwError . SizeError
               $ printf "PrimitiveArrays would be larger than maximal cell size. The given limit is %d, but the requested size is %d, with size %s for each array. (Debug hint: %s)"
                  maxCells s (show ps) (show s)
  return . CellSize $ fromIntegral s
{-# Inlinable sizeIsValid #-}

-- | In case @totalSize@ or variants thereof produce a size that is too big to
-- handle.

newtype SizeError = SizeError String
  deriving (Eq,Ord,Show)

  -- | The total number of cells that are allocated.

newtype CellSize = CellSize Word
  deriving (Eq,Ord,Show,Num,Bounded,Integral,Real,Enum)



-- | Generate a stream of indices in correct order for dynamic programming.
-- Since the stream generators require @concatMap@ / @flatten@ we have to
-- write more specialized code for @(z:.IX)@ stuff.

class (Index i) ⇒ IndexStream i where
  -- | Generate an index stream using 'LimitType's. This prevents having to
  -- figure out how the actual limits for complicated index types (like @Set@)
  -- would look like, since for @Set@, for example, the @LimitType Set == Int@
  -- provides just the number of bits.
  --
  -- This generates an index stream suitable for @forward@ structure filling.
  -- The first index is the smallest (or the first indices considered are all
  -- equally small in partially ordered sets). Larger indices follow up until
  -- the largest one.
  streamUp ∷ Monad m ⇒ LimitType i → LimitType i → Stream m i
  -- | If 'streamUp' generates indices from smallest to largest, then
  -- 'streamDown' generates indices from largest to smallest. Outside grammars
  -- make implicit use of this. Asking for an axiom in backtracking requests
  -- the first element from this stream.
  streamDown ∷ Monad m ⇒ LimitType i → LimitType i → Stream m i



instance Index Z where
  data LimitType Z = ZZ
  linearIndex _ _ = 0
  {-# INLINE linearIndex #-}
  size _ = 1
  {-# INLINE size #-}
  inBounds _ _ = True
  {-# INLINE inBounds #-}
  zeroBound = Z
  {-# Inline zeroBound #-}
  zeroBound' = ZZ
  {-# Inline zeroBound' #-}
  totalSize ZZ = [1]
  {-# Inline [1] totalSize #-}

instance IndexStream Z where
  streamUp ZZ ZZ = SM.singleton Z
  {-# Inline streamUp #-}
  streamDown ZZ ZZ = SM.singleton Z
  {-# Inline streamDown #-}

instance (Index zs, Index z) => Index (zs:.z) where
  data LimitType (zs:.z) = LimitType zs :.. LimitType z
  linearIndex (hs:..h) (zs:.z) = linearIndex hs zs * size h + linearIndex h z
  {-# INLINE linearIndex #-}
  size (hs:..h) = size hs * size h
  {-# INLINE size #-}
  inBounds (hs:..h) (zs:.z) = inBounds hs zs && inBounds h z
  {-# INLINE inBounds #-}
  zeroBound = zeroBound :. zeroBound
  {-# Inline zeroBound #-}
  zeroBound' = zeroBound' :.. zeroBound'
  {-# Inline zeroBound' #-}
  totalSize (hs:..h) =
    let tshs = totalSize hs
        tsh  = totalSize h
    in tshs ++ tsh
  {-# Inline totalSize #-}

deriving instance Eq      (LimitType Z)
deriving instance Generic (LimitType Z)
deriving instance Read    (LimitType Z)
deriving instance Show    (LimitType Z)

deriving instance (Eq (LimitType zs)     , Eq (LimitType z)     ) ⇒ Eq      (LimitType (zs:.z))
deriving instance (Generic (LimitType zs), Generic (LimitType z)) ⇒ Generic (LimitType (zs:.z))
deriving instance (Read (LimitType zs)   , Read (LimitType z)   ) ⇒ Read    (LimitType (zs:.z))
deriving instance (Show (LimitType zs)   , Show (LimitType z)   ) ⇒ Show    (LimitType (zs:.z))

--instance (Index zs, Index z) => Index (zs:>z) where
--  type LimitType (zs:>z) = LimitType zs:>LimitType z
--  linearIndex (hs:>h) (zs:>z) = linearIndex hs zs * (size (Proxy ∷ Proxy z) h) + linearIndex h z
--  {-# INLINE linearIndex #-}
--  size Proxy (ss:>s) = size (Proxy ∷ Proxy zs) ss * (size (Proxy ∷ Proxy z) s)
--  {-# INLINE size #-}
--  inBounds (hs:>h) (zs:>z) = inBounds hs zs && inBounds h z
--  {-# INLINE inBounds #-}

