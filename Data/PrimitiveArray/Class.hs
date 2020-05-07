
-- | Vastly extended primitive arrays. Some basic ideas are now modeled after the vector package,
-- especially the monadic mutable / pure immutable array system.
--
-- Note that in general only bulk operations should error out, error handling for index/read/write
-- is too costly. General usage should be to create data structures and run the DP code within an
-- error monad, but keep error handling to high-level operations.

module Data.PrimitiveArray.Class where

import           Control.Applicative (Applicative, pure, (<$>), (<*>))
import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Monad (forM_)
import           Control.Monad.Primitive (PrimMonad, liftPrim)
import           Control.Monad.ST (runST)
import           Data.Proxy
import           Data.Vector.Fusion.Util
import           Debug.Trace
import           GHC.Generics (Generic)
import           Prelude as P
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           GHC.Stack
import           Data.Kind (Constraint)

import           Data.PrimitiveArray.Index.Class



-- | Mutable version of an array.

data family MutArr (m :: * -> *) (arr :: *) :: *

-- | Associate a fill structure with each type of array (dense, sparse, ...).
--
-- Example: @type instance FillStruc (Sparse w v sh e) = (w sh)@ associates the type @(w sh)@, which
-- is of the same type as the underlying @w@ structure holding index information for a sparse array.

type family FillStruc arr :: *



-- | The core set of operations for pure and monadic arrays.

class (Index sh) => PrimArrayOps arr sh elm where

  -- ** Pure operations

  -- | Returns the bounds of an immutable array, again inclusive bounds: @ [lb..ub] @.
  upperBound :: arr sh elm -> LimitType sh

  -- | Extract a single element from the array. Generally unsafe as not bounds-checking is
  -- performed.
  unsafeIndex :: arr sh elm -> sh -> elm

  -- | Index into immutable array, but safe in case @sh@ is not part of the array.
  safeIndex :: arr sh elm -> sh -> Maybe elm

  -- | Savely transform the shape space of a table.
  transformShape :: Index sh' => (LimitType sh -> LimitType sh') -> arr sh elm -> arr sh' elm

  -- ** Monadic operations

  -- | Return the bounds of the array. All bounds are inclusive, as in @[lb..ub]@. Technically not
  -- monadic, but rather working on a monadic array.
  upperBoundM :: MutArr m (arr sh elm) -> LimitType sh

  -- | Given lower and upper bounds and a list of /all/ elements, produce a mutable array.
  fromListM :: PrimMonad m => LimitType sh -> [elm] -> m (MutArr m (arr sh elm))

  -- | Creates a new array with the given bounds with each element within the array being in an
  -- undefined state.
  newM :: PrimMonad m => LimitType sh -> m (MutArr m (arr sh elm))

  -- | Variant of 'newM' that requires a fill structure. Mostly for special / sparse structures
  -- (hence the @S@, also to be interpreted as "safe", since these functions won't fail with sparse
  -- structures).
  newSM :: (Monad m, PrimMonad m) => LimitType sh -> FillStruc (arr sh elm) -> m (MutArr m (arr sh elm))

  -- | Creates a new array with all elements being equal to 'elm'.
  newWithM :: PrimMonad m => LimitType sh -> elm -> m (MutArr m (arr sh elm))

  -- | Variant of 'newWithM'
  newWithSM :: (Monad m, PrimMonad m) => LimitType sh -> FillStruc (arr sh elm) -> elm -> m (MutArr m (arr sh elm))

  -- | Reads a single element in the array.
  readM :: PrimMonad m => MutArr m (arr sh elm) -> sh -> m elm

  -- | Read from the mutable array, but return @Nothing@ in case @sh@ does not exist. This will
  -- allow streaming DP combinators to "jump" over missing elements.
  --
  -- Should be used with @Stream.Monadic.mapMaybe@ to get efficient code.
  safeReadM :: (Monad m, PrimMonad m) => MutArr m (arr sh elm) -> sh -> m (Maybe elm)

  -- | Writes a single element in the array.
  writeM :: PrimMonad m => MutArr m (arr sh elm) -> sh -> elm -> m ()

  -- | Write into the mutable array, but if the index @sh@ does not exist, silently continue.
  safeWriteM :: (Monad m, PrimMonad m) => MutArr m (arr sh elm) -> sh -> elm -> m ()

  -- | Freezes a mutable array an returns its immutable version. This operation is /O(1)/ and both
  -- arrays share the same memory. Do not use the mutable array afterwards.
  unsafeFreezeM :: PrimMonad m => MutArr m (arr sh elm) -> m (arr sh elm)

  -- | Thaw an immutable array into a mutable one. Both versions share memory.
  unsafeThawM :: PrimMonad m => arr sh elm -> m (MutArr m (arr sh elm))


class PrimArrayMap arr sh e e' where
  -- -- | Map a function of type @elm -> e@ over the primitive array, returning another primitive array
  -- -- of same type and shape but different element.
  mapArray :: (e -> e') -> arr sh e -> arr sh e'


-- | Sum type of errors that can happen when using primitive arrays.

data PAErrors
  = PAEUpperBound
  deriving stock (Eq,Generic)

instance Show PAErrors where
  show (PAEUpperBound) = "Upper bound is too large for @Int@ size!"



-- | Infix index operator. Performs minimal bounds-checking using assert in non-optimized code.
--
-- @(!)@ is rewritten from phase @[1]@ onwards into an optimized form. Before, it uses a very slow
-- form, that does bounds checking.

--(!) :: (HasCallStack, PrimArrayOps arr sh elm) => arr sh elm -> sh -> elm
(!) :: (PrimArrayOps arr sh elm) => arr sh elm -> sh -> elm
{-# Inline [1] (!) #-}
{-# Rules "unsafeIndex" [2] (!) = unsafeIndex #-}
(!) = \arr idx -> case safeIndex arr idx of
          Nothing -> error $ show (showBound (upperBound arr), showIndex idx)
          Just v  -> v



-- | Return value at an index that might not exist.

(!?) :: PrimArrayOps arr sh elm => arr sh elm -> sh -> Maybe elm
{-# Inline (!?) #-}
(!?) = safeIndex

-- | Returns true if the index is valid for the array.

inBoundsM :: (Monad m, PrimArrayOps arr sh elm) => MutArr m (arr sh elm) -> sh -> Bool
inBoundsM marr idx = inBounds (upperBoundM marr) idx
{-# INLINE inBoundsM #-}

-- -- | Given two arrays with the same dimensionality, their respective starting
-- -- index, and how many steps to go in each dimension (in terms of a dimension
-- -- again), determine if the multidimensional slices have the same value at
-- -- all positions
-- --
-- -- TODO specialize for DIM1 (and maybe higher dim's) to use memcmp
-- 
-- sliceEq :: (Eq elm, PrimArrayOps arr sh elm) => arr sh elm -> sh -> arr sh elm -> sh -> sh -> Bool
-- sliceEq arr1 k1 arr2 k2 xtnd = assert ((inBounds arr1 k1) && (inBounds arr2 k2) && (inBounds arr1 $ k1 `addDim` xtnd) && (inBounds arr2 $ k2 `addDim` xtnd)) $ and res where
--   res = zipWith (==) xs ys
--   xs = P.map (unsafeIndex arr1) $ rangeList k1 xtnd
--   ys = P.map (unsafeIndex arr2) $ rangeList k2 xtnd
-- {-# INLINE sliceEq #-}

-- | Construct a mutable primitive array from a lower and an upper bound, a
-- default element, and a list of associations.

fromAssocsM
  :: (PrimMonad m, PrimArrayOps arr sh elm)
  => LimitType sh -> elm -> [(sh,elm)] -> m (MutArr m (arr sh elm))
fromAssocsM ub def xs = do
  ma <- newWithM ub def
--  let s = size ub
--  traceShow (s,length xs) $ when (s < length xs) $ error "bang"
  forM_ xs $ \(k,v) -> writeM ma k v
  return ma
{-# INLINE fromAssocsM #-}

-- | Initialize an immutable array but stay within the primitive monad @m@.

newWithPA
  :: (PrimMonad m, PrimArrayOps arr sh elm)
  => LimitType sh
  -> elm
  -> m (arr sh elm)
newWithPA ub def = do
  ma ← newWithM ub def
  unsafeFreezeM ma
{-# Inlinable newWithPA #-}

-- | Initialize an immutable array with a fill structure.

newWithSPA
  ∷ (PrimMonad m, PrimArrayOps arr sh elm)
  ⇒ LimitType sh
  -> FillStruc (arr sh elm)
  → elm
  → m (arr sh elm)
{-# Inlinable newWithSPA #-}
newWithSPA ub xs def = do
  ma ← newWithSM ub xs def
  unsafeFreezeM ma

-- | Safely prepare a primitive array.
--
-- TODO Check if having a 'MonadError' instance degrades performance. (We
-- should see this once the test with NeedlemanWunsch is under way).

safeNewWithPA
  :: forall m arr sh elm
  . (PrimMonad m, MonadError PAErrors m, PrimArrayOps arr sh elm)
  => LimitType sh
  -> elm
  -> m (arr sh elm)
safeNewWithPA ub def = do
  case runExcept $ sizeIsValid maxBound [totalSize ub] of
    Left  (SizeError _) -> throwError PAEUpperBound
    Right (CellSize  _) -> newWithPA ub def
{-# Inlinable safeNewWithPA #-}


-- | Return all associations from an array.

assocs :: forall arr sh elm . (IndexStream sh, PrimArrayOps arr sh elm) => arr sh elm -> [(sh,elm)]
assocs arr = unId . SM.toList $ assocsS arr
{-# INLINE assocs #-}

-- | Return all associations from an array.

assocsS :: forall m arr sh elm . (Monad m, IndexStream sh, PrimArrayOps arr sh elm) => arr sh elm -> SM.Stream m (sh,elm)
assocsS arr = SM.map (\k -> (k,unsafeIndex arr k)) $ streamUp zeroBound' (upperBound arr)
{-# INLINE assocsS #-}

-- | Creates an immutable array from lower and upper bounds and a complete list
-- of elements.

fromList :: (PrimArrayOps arr sh elm) => LimitType sh -> [elm] -> arr sh elm
fromList ub xs = runST $ fromListM ub xs >>= unsafeFreezeM
{-# INLINE fromList #-}

-- | Creates an immutable array from lower and upper bounds, a default element,
-- and a list of associations.

fromAssocs :: (PrimArrayOps arr sh elm) => LimitType sh -> elm -> [(sh,elm)] -> arr sh elm
fromAssocs ub def xs = runST $ fromAssocsM ub def xs >>= unsafeFreezeM
{-# INLINE fromAssocs #-}

-- -- | Determines if an index is valid for a given immutable array.
-- 
-- inBounds :: PrimArrayOps arr sh elm => arr sh elm -> sh -> Bool
-- inBounds arr idx = let (lb,ub) = bounds arr in inShapeRange lb (ub `addDim` unitDim) idx
-- {-# INLINE inBounds #-}

-- | Returns all elements of an immutable array as a list.

toList :: forall arr sh elm . (IndexStream sh, PrimArrayOps arr sh elm) => arr sh elm -> [elm]
toList arr = let ub = upperBound arr in P.map ((!) arr) . unId . SM.toList $ streamUp zeroBound' ub
{-# INLINE toList #-}



{-

-- * Freeze an inductive stack of tables with a 'Z' at the bottom.

-- | 'freezeTables' freezes a stack of tables.

class FreezeTables m t where
    type Frozen t :: *
    freezeTables :: t -> m (Frozen t)

instance Applicative m => FreezeTables m Z where
    type Frozen Z = Z
    freezeTables Z = pure Z
    {-# INLINE freezeTables #-}

instance (Functor m, Applicative m, Monad m, PrimMonad m, FreezeTables m ts, PrimArrayOps arr sh elm) => FreezeTables m (ts:.MutArr m (arr sh elm)) where
    type Frozen (ts:.MutArr m (arr sh elm)) = Frozen ts :. arr sh elm
    freezeTables (ts:.t) = (:.) <$> freezeTables ts <*> unsafeFreezeM t
    {-# INLINE freezeTables #-}

-}

