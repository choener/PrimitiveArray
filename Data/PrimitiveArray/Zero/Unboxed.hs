{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Strict, unboxed arrays of primitive type. Uses unboxed vectors internally
-- to provide tuple instances.

module Data.PrimitiveArray.Zero.Unboxed where

import Control.Monad
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Control.Exception (assert)
import Data.Vector.Unboxed as VU hiding (forM_, length, zipWithM_)
import Data.Vector.Unboxed.Mutable as VUM hiding (length)

import Data.ExtShape
import Data.PrimitiveArray



-- | Monadic arrays of primitive type.

data MArr0 s sh elm = MArr0 !sh !(MVector s elm)

-- | Immutable arrays of primitive type.

data Arr0 sh elm = Arr0 !sh !(Vector elm)



type instance MutArray Arr0 = MArr0

-- NOTE inLb, inUb is including bound, while exUb is excluding upper bound.
-- Differentiates between largest included index, first excluded index.

instance (Shape sh, ExtShape sh, VU.Unbox elm) => MPrimArrayOps MArr0 sh elm where
  boundsM (MArr0 exUb _) = (zeroDim,exUb `subDim` unitDim)
  fromListM inLb inUb xs = do
    ma <- newM inLb inUb
    let exUb = inUb `addDim` unitDim
    let (MArr0 _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size exUb) $ unsafeWrite mba k x) [0.. toIndex exUb inUb] xs
    return ma
  newM inLb inUb = let exUb = inUb `addDim` unitDim in
    unless (inLb == zeroDim) (error "MArr0 lb/=zeroDim") >>
    MArr0 exUb `liftM` new (size exUb)
  newWithM inLb inUb def = do
    let exUb = inUb `addDim` unitDim
    ma <- newM inLb inUb
    let (MArr0 _ mba) = ma
    forM_ [0 .. toIndex exUb inUb] $ \k -> unsafeWrite mba k def
    return ma
  readM (MArr0 exUb mba) idx = assert (inShape exUb idx) $ unsafeRead mba (toIndex exUb idx)
  writeM (MArr0 exUb mba) idx elm = assert (inShape exUb idx) $ unsafeWrite mba (toIndex exUb idx) elm
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Shape sh, ExtShape sh, VUM.Unbox elm) => PrimArrayOps Arr0 sh elm where
  bounds (Arr0 exUb _) = (zeroDim,exUb `subDim` unitDim)
  freeze (MArr0 exUb mba) = Arr0 exUb `liftM` unsafeFreeze mba
  index (Arr0 exUb ba) idx = assert (inShape exUb idx) $ unsafeIndex ba (toIndex exUb idx)
  {-# INLINE bounds #-}
  {-# INLINE freeze #-}
  {-# INLINE index #-}

