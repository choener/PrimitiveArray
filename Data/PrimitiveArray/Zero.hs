{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Boxed, primitive arrays. A good use-case is to store boxed or unboxed
-- vectors.

module Data.PrimitiveArray.Zero where

import Control.Monad
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Primitive
import Data.Primitive.Array
import Data.Primitive.Types
import Control.Exception (assert)

import Data.ExtShape
import Data.PrimitiveArray



-- | Monadic arrays of primitive type.

data MArr0 s sh elm = MArr0 !sh {-# UNPACK #-} !(MutableArray s elm)

-- | Immutable arrays of primitive type.

data Arr0 sh elm = Arr0 !sh {-# UNPACK #-} !(Array elm)



type instance MutArray Arr0 = MArr0

-- NOTE inLb, inUb is including bound, while exUb is excluding upper bound.
-- Differentiates between largest included index, first excluded index.

instance (Shape sh, ExtShape sh) => MPrimArrayOps MArr0 sh elm where
  boundsM (MArr0 exUb _) = (zeroDim,exUb `subDim` unitDim)
  fromListM inLb inUb xs = do
    ma <- newM inLb inUb
    let exUb = inUb `addDim` unitDim
    let (MArr0 _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size exUb) $ writeArray mba k x) [0.. toIndex exUb inUb] xs
    return ma
  newM inLb inUb = let exUb = inUb `addDim` unitDim in
    unless (inLb == zeroDim) (error "MArr0 lb/=zeroDim") >>
    MArr0 exUb `liftM` newArray (size exUb) undefined
  newWithM inLb inUb def = do
    let exUb = inUb `addDim` unitDim
    ma <- newM inLb inUb
    let (MArr0 _ mba) = ma
    forM_ [0 .. toIndex exUb inUb] $ \k -> writeArray mba k def
    return ma
  readM (MArr0 exUb mba) idx = assert (inShape exUb idx) $ readArray mba (toIndex exUb idx)
  writeM (MArr0 exUb mba) idx elm = assert (inShape exUb idx) $ writeArray mba (toIndex exUb idx) elm
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Shape sh, ExtShape sh) => PrimArrayOps Arr0 sh elm where
  bounds (Arr0 exUb _) = (zeroDim,exUb `subDim` unitDim)
  freeze (MArr0 exUb mba) = Arr0 exUb `liftM` unsafeFreezeArray mba
  index (Arr0 exUb ba) idx = assert (inShape exUb idx) $ indexArray ba (toIndex exUb idx)
  {-# INLINE bounds #-}
  {-# INLINE freeze #-}
  {-# INLINE index #-}

