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
-- NOTE all operations in OpsM and Ops are highly unsafe. No bounds-checking is
-- performed at all.

module Data.PrimitiveArray where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Primitive.Types
import Data.Primitive
import Control.Monad.ST
import Control.Monad
import Control.Monad.Primitive
import System.IO.Unsafe

import Data.ExtShape



class (Shape sh, ExtShape sh) => OpsM arrm sh elm where
  boundsM :: PrimMonad m => arrm (PrimState m) sh elm -> (sh,sh)
  fromListM :: PrimMonad m => sh -> sh -> [elm] -> m (arrm (PrimState m) sh elm)
  inBoundsM :: PrimMonad m => arrm (PrimState m) sh elm -> sh -> Bool
  newM :: PrimMonad m => sh -> sh -> m (arrm (PrimState m) sh elm)
  newWithM :: PrimMonad m => sh -> sh -> elm -> m (arrm (PrimState m) sh elm)
  readM :: PrimMonad m => arrm (PrimState m) sh elm -> sh -> m elm
  writeM :: PrimMonad m => arrm (PrimState m) sh elm -> sh -> elm -> m ()

type family Mut (v :: * -> * -> * ) :: * -> * -> * -> *

class (Shape sh, ExtShape sh, OpsM (Mut arr) sh elm) => Ops arr sh elm where
  bounds :: arr sh elm -> (sh,sh)
  freeze :: PrimMonad m => Mut arr (PrimState m) sh elm -> m (arr sh elm)
  inBounds :: arr sh elm -> sh -> Bool
  index :: arr sh elm -> sh -> elm
  toList :: arr sh elm -> [elm]

-- | Given two arrays with the same dimensionality, their respective starting
-- index, and how many steps to go in each dimension (in terms of a dimension
-- again), determine if the multidimensional slices have the same value at
-- all positions
--
-- TODO specialize for DIM1 (and maybe higher dim's) to use memcmp

sliceEq :: (Eq elm, Ops arr sh elm) => arr sh elm -> sh -> arr sh elm -> sh -> sh -> Bool
sliceEq arr1 k1 arr2 k2 xtnd = and res where
  res = zipWith (==) xs ys
  xs = map (index arr1) $ rangeList k1 xtnd
  ys = map (index arr2) $ rangeList k2 xtnd
{-# INLINE sliceEq #-}

-- | Construct a mutable primitive array from a lower and an upper bound, a
-- default element, and a list of associations.

fromAssocsM
  :: (PrimMonad m, OpsM arrm sh elm)
  => sh -> sh -> elm -> [(sh,elm)] -> m (arrm (PrimState m) sh elm)
fromAssocsM lb ub def xs = do
  ma <- newWithM lb ub def
  forM_ xs $ \(k,v) -> writeM ma k v
  return ma
{-# INLINE fromAssocsM #-}

-- | Return all associations from an array.

assocs :: Ops arr sh elm => arr sh elm -> [(sh,elm)]
assocs arr = map (\k -> (k,index arr k)) $ rangeList lb (ub `subDim` lb) where
  (lb,ub) = bounds arr
{-# INLINE assocs #-}

fromList :: Ops arr sh elm => sh -> sh -> [elm] -> arr sh elm
fromList lb ub xs = runST $ fromListM lb ub xs >>= freeze
{-# INLINE fromList #-}

fromAssocs :: Ops arr sh elm => sh -> sh -> elm -> [(sh,elm)] -> arr sh elm
fromAssocs lb ub def xs = runST $ fromAssocsM lb ub def xs >>= freeze
{-# INLINE fromAssocs #-}



data MArr0 s sh elm = MArr0 sh (MutableByteArray s)

data Arr0 sh elm = Arr0 sh ByteArray

type instance Mut Arr0 = MArr0

instance (Shape sh, ExtShape sh, Prim elm) => OpsM MArr0 sh elm where
  boundsM (MArr0 ub _) = (zeroDim,ub `subDim` unitDim)
  fromListM lb ub xs = do
    ma <- newM lb ub
    let (MArr0 _ mba) = ma
    zipWithM_ (\k x -> writeByteArray mba k x) [0..] xs
    return ma
  inBoundsM (MArr0 ub _) idx = inShapeRange zeroDim ub idx
  newM lb ub' = let ub = ub' `addDim` unitDim in
    unless (lb == zeroDim) (error "MArr0 lb/=zeroDim") >>
    MArr0 ub `liftM` newByteArray (size ub * sizeOf (undefined :: elm))
  newWithM lb ub def = do
    let ubreal = ub `addDim` unitDim
    ma <- newM lb ub
    let (MArr0 _ mba) = ma
    forM_ [0 .. (toIndex ubreal ub)] $ \k -> writeByteArray mba k def
    return ma
  readM (MArr0 ub mba) idx = readByteArray mba (toIndex ub idx)
  writeM (MArr0 ub mba) idx elm = writeByteArray mba (toIndex ub idx) elm

instance (Shape sh, ExtShape sh, Prim elm) => Ops Arr0 sh elm where
  freeze (MArr0 ub mba) = Arr0 ub `liftM` unsafeFreezeByteArray mba
  index (Arr0 ub ba) idx = indexByteArray ba (toIndex ub idx)

test :: Arr0 DIM2 Int
test = fromList zeroDim unitDim [0,1,2,3]

{-
data MArr s sh elm = MArr sh sh (MutableByteArray s)

data Arr sh elm = Arr sh sh ByteArray

type instance Mut Arr = MArr

instance (Shape sh, Prim elm) => OpsM MArr sh elm where
  unsafeNewM lb ub' = let ub = ub' `addDim` unitDim in
    MArr lb ub `liftM` newByteArray ((size ub - size lb) * sizeOf (undefined :: elm))

instance (Shape sh, Prim elm) => Ops Arr sh elm where
  unsafeIndex (Arr lb ub ba) idx = undefined
  unsafeFreeze (MArr lb ub mba) = Arr lb ub `liftM` unsafeFreezeByteArray mba
-}


