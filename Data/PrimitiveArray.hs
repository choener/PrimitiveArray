{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Vastly extended primitive arrays. Some basic ideas are now modeled after
-- the vector package, especially the monadic mutable / pure immutable array
-- system. There are eight flavors of arrays among three axes: mutable/pure +
-- boxed/unboxed + zero-based/lower-bound.
--
-- NOTE all operations in MPrimArrayOps and PrimArrayOps are highly unsafe. No
-- bounds-checking is performed at all.

module Data.PrimitiveArray where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Primitive.Types
import Data.Primitive
import Control.Monad.ST
import Control.Monad
import Control.Monad.Primitive
import System.IO.Unsafe
import Control.Exception (assert)

import Data.ExtShape



-- | The core set of operations for monadic arrays.

class (Shape sh, ExtShape sh) => MPrimArrayOps marr sh elm where

  -- | Return the bounds of the array. All bounds are inclusive, as in
  -- @[lb..ub]@

  boundsM :: marr s sh elm -> (sh,sh)

  -- | Given lower and upper bounds and a list of /all/ elements, produce a
  -- mutable array.

  fromListM :: PrimMonad m => sh -> sh -> [elm] -> m (marr (PrimState m) sh elm)

  -- | Creates a new array with the given bounds with each element within the
  -- array being in a random state.

  newM :: PrimMonad m => sh -> sh -> m (marr (PrimState m) sh elm)

  -- | Creates a new array with all elements being equal to 'elm'.

  newWithM :: PrimMonad m => sh -> sh -> elm -> m (marr (PrimState m) sh elm)

  -- | Reads a single element in the array.

  readM :: PrimMonad m => marr (PrimState m) sh elm -> sh -> m elm

  -- | Writes a single element in the array.

  writeM :: PrimMonad m => marr (PrimState m) sh elm -> sh -> elm -> m ()



-- | Used to connect each immutable array with one mutable array.

type family MutArray (v :: * -> * -> * ) :: * -> * -> * -> *



-- | The core set of functions on immutable arrays.

class (Shape sh, ExtShape sh, MPrimArrayOps (MutArray arr) sh elm) => PrimArrayOps arr sh elm where

  -- | Returns the bounds of an immutable array, again inclusive bounds: @ [lb..ub] @.

  bounds :: arr sh elm -> (sh,sh)

  -- | Freezes a mutable array an returns its immutable version. This operation
  -- is /O(1)/ and both arrays share the same memory. Do not use the mutable
  -- array afterwards.

  freeze :: PrimMonad m => MutArray arr (PrimState m) sh elm -> m (arr sh elm)

  -- | Extract a single element from the array. Generally unsafe as not
  -- bounds-checking is performed.

  index :: arr sh elm -> sh -> elm



-- | Infix index operator. Performs minimal bounds-checking using assert in
-- non-optimized code.

(!) :: PrimArrayOps arr sh elm => arr sh elm -> sh -> elm
(!) arr idx = assert (inBounds arr idx) $ index arr idx
{-# INLINE (!) #-}

-- | Returns true if the index is valid for the array.
--
-- TODO can't give a typedef

inBoundsM :: MPrimArrayOps marr sh elm => marr s sh elm -> sh -> Bool
inBoundsM marr idx = let (lb,ub) = boundsM marr in inShapeRange lb ub idx
{-# INLINE inBoundsM #-}

-- | Given two arrays with the same dimensionality, their respective starting
-- index, and how many steps to go in each dimension (in terms of a dimension
-- again), determine if the multidimensional slices have the same value at
-- all positions
--
-- TODO specialize for DIM1 (and maybe higher dim's) to use memcmp

sliceEq :: (Eq elm, PrimArrayOps arr sh elm) => arr sh elm -> sh -> arr sh elm -> sh -> sh -> Bool
sliceEq arr1 k1 arr2 k2 xtnd = assert ((inBounds arr1 k1) && (inBounds arr2 k2) && (inBounds arr1 $ k1 `addDim` xtnd) && (inBounds arr2 $ k2 `addDim` xtnd)) $ and res where
  res = zipWith (==) xs ys
  xs = map (index arr1) $ rangeList k1 xtnd
  ys = map (index arr2) $ rangeList k2 xtnd
{-# INLINE sliceEq #-}

-- | Construct a mutable primitive array from a lower and an upper bound, a
-- default element, and a list of associations.

fromAssocsM
  :: (PrimMonad m, MPrimArrayOps marr sh elm)
  => sh -> sh -> elm -> [(sh,elm)] -> m (marr (PrimState m) sh elm)
fromAssocsM lb ub def xs = do
  ma <- newWithM lb ub def
  forM_ xs $ \(k,v) -> writeM ma k v
  return ma
{-# INLINE fromAssocsM #-}

-- | Return all associations from an array.

assocs :: PrimArrayOps arr sh elm => arr sh elm -> [(sh,elm)]
assocs arr = map (\k -> (k,index arr k)) $ rangeList lb (ub `subDim` lb) where
  (lb,ub) = bounds arr
{-# INLINE assocs #-}

-- | Creates an immutable array from lower and upper bounds and a complete list
-- of elements.

fromList :: PrimArrayOps arr sh elm => sh -> sh -> [elm] -> arr sh elm
fromList lb ub xs = runST $ fromListM lb ub xs >>= freeze
{-# INLINE fromList #-}

-- | Creates an immutable array from lower and upper bounds, a default element,
-- and a list of associations.

fromAssocs :: PrimArrayOps arr sh elm => sh -> sh -> elm -> [(sh,elm)] -> arr sh elm
fromAssocs lb ub def xs = runST $ fromAssocsM lb ub def xs >>= freeze
{-# INLINE fromAssocs #-}

-- | Determines if an index is valid for a given immutable array.

inBounds :: PrimArrayOps arr sh elm => arr sh elm -> sh -> Bool
inBounds arr idx = let (lb,ub) = bounds arr in inShapeRange lb ub idx
{-# INLINE inBounds #-}

-- | Returns all elements of an immutable array as a list.

toList :: PrimArrayOps arr sh elm =>  arr sh elm -> [elm]
toList arr = let (lb,ub) = bounds arr in map ((!) arr) $ rangeList lb $ ub `subDim` lb
{-# INLINE toList #-}

