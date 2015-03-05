{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Strict, unboxed arrays of primitive type.

module Data.PrimitiveArray.Unboxed.Zero where

import Control.Monad
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Primitive
import Data.Primitive.Types
import Control.Exception (assert)

import Data.ExtShape
import Data.PrimitiveArray



-- | Monadic arrays of primitive type.

data MArr0 s sh elm = MArr0 !sh !(MutableByteArray s)

-- | Immutable arrays of primitive type.

data Arr0 sh elm = Arr0 !sh !ByteArray



type instance MutArray Arr0 = MArr0

-- NOTE inLb, inUb is including bound, while exUb is excluding upper bound.
-- Differentiates between largest included index, first excluded index.

instance (Shape sh, ExtShape sh, Prim elm) => MPrimArrayOps MArr0 sh elm where
  boundsM (MArr0 exUb _) = (zeroDim,exUb `subDim` unitDim)
  fromListM inLb inUb xs = do
    ma <- newM inLb inUb
    let exUb = inUb `addDim` unitDim
    let (MArr0 _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size exUb) $ writeByteArray mba k x) [0.. toIndex exUb inUb] xs
    return ma
  newM inLb inUb = let exUb = inUb `addDim` unitDim in
    unless (inLb == zeroDim) (error "MArr0 lb/=zeroDim") >>
    MArr0 exUb `liftM` newByteArray (size exUb * sizeOf (undefined :: elm))
  newWithM inLb inUb def = do
    let exUb = inUb `addDim` unitDim
    ma <- newM inLb inUb
    let (MArr0 _ mba) = ma
    forM_ [0 .. toIndex exUb inUb] $ \k -> writeByteArray mba k def
    return ma
  readM (MArr0 exUb mba) idx = assert (inShape exUb idx) $ readByteArray mba (toIndex exUb idx)
  writeM (MArr0 exUb mba) idx elm = assert (inShape exUb idx) $ writeByteArray mba (toIndex exUb idx) elm
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Shape sh, ExtShape sh, Prim elm) => PrimArrayOps Arr0 sh elm where
  bounds (Arr0 exUb _) = (zeroDim,exUb `subDim` unitDim)
  freeze (MArr0 exUb mba) = Arr0 exUb `liftM` unsafeFreezeByteArray mba
  index (Arr0 exUb ba) idx = assert (inShape exUb idx) $ indexByteArray ba (toIndex exUb idx)
  {-# INLINE bounds #-}
  {-# INLINE freeze #-}
  {-# INLINE index #-}

