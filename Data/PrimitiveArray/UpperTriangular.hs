{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Upper triangular arrays. We assume that the lower bound is at 0 (or 0,0 or
-- 0,0,0 ...)

module Data.PrimitiveArray.UpperTriangular where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import qualified Data.Vector as V

import Data.ExtShape
import Data.PrimitiveArray



data UpperT sh elm = UpperT !sh !(V.Vector elm)

data MUpperT s sh elm = MUpperT  !sh !(V.MVector s elm)

type instance MutArray UpperT = MUpperT



instance (Shape sh, ExtShape sh) => MPrimArrayOps MUpperT sh elm where
  boundsM (MUpperT exUb _) = (zeroDim,exUb `subDim` unitDim)
  {-
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
-}

