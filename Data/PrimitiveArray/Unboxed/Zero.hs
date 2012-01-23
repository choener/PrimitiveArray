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

import Data.ExtShape
import Data.PrimitiveArray



-- | Monadic arrays of primitive type.

data MArr0 s sh elm = MArr0 {-# UNPACK #-} !sh {-# UNPACK #-} !(MutableByteArray s)

-- | Immutable arrays of primitive type.

data Arr0 sh elm = Arr0 {-# UNPACK #-} !sh {-# UNPACK #-} !ByteArray



type instance MutArray Arr0 = MArr0

instance (Shape sh, ExtShape sh, Prim elm) => MPrimArrayOps MArr0 sh elm where
  boundsM (MArr0 ub _) = (zeroDim,ub `subDim` unitDim)
  fromListM lb ub xs = do
    ma <- newM lb ub
    let ubreal = ub `addDim` unitDim
    let (MArr0 _ mba) = ma
    zipWithM_ (\k x -> writeByteArray mba k x) [0.. size ubreal] xs
    return ma
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
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Shape sh, ExtShape sh, Prim elm) => PrimArrayOps Arr0 sh elm where
  bounds (Arr0 ub _) = (zeroDim,ub `subDim` unitDim)
  freeze (MArr0 ub mba) = Arr0 ub `liftM` unsafeFreezeByteArray mba
  index (Arr0 ub ba) idx = indexByteArray ba (toIndex ub idx)
  {-# INLINE bounds #-}
  {-# INLINE freeze #-}
  {-# INLINE index #-}

