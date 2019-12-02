
-- | Vastly extended primitive arrays. Some basic ideas are now modeled after
-- the vector package, especially the monadic mutable / pure immutable array
-- system.
--
-- NOTE all operations in MPrimArrayOps and PrimArrayOps are highly unsafe. No
-- bounds-checking is performed at all.

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

import           Data.PrimitiveArray.Index.Class



-- | Mutable version of an array.

data family MutArr (m :: * -> *) (arr :: *) :: *


-- | The core set of operations for monadic arrays.

class (Index sh) => MPrimArrayOps arr sh elm where

  -- | Return the bounds of the array. All bounds are inclusive, as in
  -- @[lb..ub]@

  upperBoundM :: MutArr m (arr sh elm) -> LimitType sh

  -- | Given lower and upper bounds and a list of /all/ elements, produce a
  -- mutable array.

  fromListM :: PrimMonad m => LimitType sh -> [elm] -> m (MutArr m (arr sh elm))

  -- | Creates a new array with the given bounds with each element within the
  -- array being in an undefined state.

  newM :: PrimMonad m => LimitType sh -> m (MutArr m (arr sh elm))

  -- | Creates a new array with all elements being equal to 'elm'.

  newWithM :: PrimMonad m => LimitType sh -> elm -> m (MutArr m (arr sh elm))

  -- | Reads a single element in the array.

  readM :: PrimMonad m => MutArr m (arr sh elm) -> sh -> m elm

  -- | Writes a single element in the array.

  writeM :: PrimMonad m => MutArr m (arr sh elm) -> sh -> elm -> m ()



-- | The core set of functions on immutable arrays.

class (Index sh) => PrimArrayOps arr sh elm where

  -- | Returns the bounds of an immutable array, again inclusive bounds: @ [lb..ub] @.

  upperBound :: arr sh elm -> LimitType sh

  -- | Freezes a mutable array an returns its immutable version. This operation
  -- is /O(1)/ and both arrays share the same memory. Do not use the mutable
  -- array afterwards.

  unsafeFreeze :: PrimMonad m => MutArr m (arr sh elm) -> m (arr sh elm)

  -- | Thaw an immutable array into a mutable one. Both versions share
  -- memory.

  unsafeThaw :: PrimMonad m => arr sh elm -> m (MutArr m (arr sh elm))

  -- | Extract a single element from the array. Generally unsafe as not
  -- bounds-checking is performed.

  unsafeIndex :: arr sh elm -> sh -> elm

  -- | Savely transform the shape space of a table.

  transformShape :: (Index sh') => (LimitType sh -> LimitType sh') -> arr sh elm -> arr sh' elm

class (Index sh) => PrimArrayMap arr sh e e' where

  -- | Map a function over each element, keeping the shape intact.

  map :: (e -> e') -> arr sh e -> arr sh e'



data PAErrors
  = PAEUpperBound
  deriving stock (Eq,Generic)

instance Show PAErrors where
  show (PAEUpperBound) = "Upper bound is too large for @Int@ size!"



-- | Infix index operator. Performs minimal bounds-checking using assert in
-- non-optimized code.
--
-- @(!)@ is rewritten from phase @[1]@ onwards into an optimized form. Before, it uses a very slow
-- form, that does bounds checking.

--(!) :: (HasCallStack, PrimArrayOps arr sh elm) => arr sh elm -> sh -> elm
(!) :: (PrimArrayOps arr sh elm) => arr sh elm -> sh -> elm
{-# Inline [1] (!) #-}
{-# Rules "unsafeIndex" [2] (!) = unsafeIndex #-}
(!) = \arr idx ->
  let a = inBounds (upperBound arr) idx
      i = unsafeIndex arr idx
  in if a then i else traceShow (showBound (upperBound arr), showIndex idx) $ assert a i



-- | Return value at an index that might not exist.

(!?) :: PrimArrayOps arr sh elm => arr sh elm -> sh -> Maybe elm
{-# Inline (!?) #-}
(!?) arr idx = if inBounds (upperBound arr) idx then Just (arr ! idx) else Nothing

-- | Returns true if the index is valid for the array.

inBoundsM :: (Monad m, MPrimArrayOps arr sh elm) => MutArr m (arr sh elm) -> sh -> Bool
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
  :: (PrimMonad m, MPrimArrayOps arr sh elm)
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
  ∷ (PrimMonad m, MPrimArrayOps arr sh elm, PrimArrayOps arr sh elm)
  ⇒ LimitType sh
  → elm
  → m (arr sh elm)
newWithPA ub def = do
  ma ← newWithM ub def
  unsafeFreeze ma
{-# Inlinable newWithPA #-}

-- | Safely prepare a primitive array.
--
-- TODO Check if having a 'MonadError' instance degrades performance. (We
-- should see this once the test with NeedlemanWunsch is under way).

safeNewWithPA
  ∷ forall m arr sh elm 
  . (PrimMonad m, MonadError PAErrors m, MPrimArrayOps arr sh elm, PrimArrayOps arr sh elm)
  ⇒ LimitType sh
  → elm
  → m (arr sh elm)
safeNewWithPA ub def = do
  case runExcept $ sizeIsValid maxBound [totalSize ub] of
    Left  (SizeError _) → throwError PAEUpperBound
    Right (CellSize  _) → newWithPA ub def
{-# Inlinable safeNewWithPA #-}


-- | Return all associations from an array.

assocs :: forall arr sh elm . (IndexStream sh, PrimArrayOps arr sh elm) => arr sh elm -> [(sh,elm)]
assocs arr = unId . SM.toList $ assocsS arr
{-# INLINE assocs #-}

-- | Return all associations from an array.

assocsS ∷ forall m arr sh elm . (Monad m, IndexStream sh, PrimArrayOps arr sh elm) ⇒ arr sh elm → SM.Stream m (sh,elm)
assocsS arr = SM.map (\k -> (k,unsafeIndex arr k)) $ streamUp zeroBound' (upperBound arr)
{-# INLINE assocsS #-}

-- | Creates an immutable array from lower and upper bounds and a complete list
-- of elements.

fromList :: (PrimArrayOps arr sh elm, MPrimArrayOps arr sh elm) => LimitType sh -> [elm] -> arr sh elm
fromList ub xs = runST $ fromListM ub xs >>= unsafeFreeze
{-# INLINE fromList #-}

-- | Creates an immutable array from lower and upper bounds, a default element,
-- and a list of associations.

fromAssocs :: (PrimArrayOps arr sh elm, MPrimArrayOps arr sh elm) => LimitType sh -> elm -> [(sh,elm)] -> arr sh elm
fromAssocs ub def xs = runST $ fromAssocsM ub def xs >>= unsafeFreeze
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
    freezeTables (ts:.t) = (:.) <$> freezeTables ts <*> unsafeFreeze t
    {-# INLINE freezeTables #-}

