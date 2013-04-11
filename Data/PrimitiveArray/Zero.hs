{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Primitive arrays where the lower index is zero (or the equivalent of zero
-- for newtypes and enumerations).

module Data.PrimitiveArray.Zero where

import Control.Monad
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Control.Exception (assert)
import qualified Data.Vector.Unboxed as VU hiding (forM_, length, zipWithM_)
import qualified Data.Vector as V hiding (forM_, length, zipWithM_)
import qualified Data.Vector.Unboxed.Mutable as VUM hiding (length)
import Data.Vector.Generic as G hiding (forM_, length, zipWithM_, new)
import Data.Vector.Generic.Mutable as GM hiding (length)
import Control.Monad.Primitive (PrimState)

import Data.Array.Repa.ExtShape
import Data.PrimitiveArray



-- * Unboxed, multidimensional arrays.

data Unboxed sh e = Unboxed !sh !(VU.Vector e)
  deriving (Read,Show,Eq)

data instance MutArr m (Unboxed sh e) = MUnboxed !sh (VU.MVector (PrimState m) e)

instance (Shape sh, ExtShape sh, VUM.Unbox elm) => MPrimArrayOps Unboxed sh elm where
  boundsM (MUnboxed exUb _) = (zeroDim,exUb `subDim` unitDim)
  fromListM inLb inUb xs = do
    ma <- newM inLb inUb
    let exUb = inUb `addDim` unitDim
    let (MUnboxed _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size exUb) $ unsafeWrite mba k x) [0.. toIndex exUb inUb] xs
    return ma
  newM inLb inUb = let exUb = inUb `addDim` unitDim in
    unless (inLb == zeroDim) (error "MArr0 lb/=zeroDim") >>
    MUnboxed exUb `liftM` new (size exUb)
  newWithM inLb inUb def = do
    let exUb = inUb `addDim` unitDim
    ma <- newM inLb inUb
    let (MUnboxed _ mba) = ma
    forM_ [0 .. toIndex exUb inUb] $ \k -> unsafeWrite mba k def
    return ma
  readM (MUnboxed exUb mba) idx = assert (inShape exUb idx) $ unsafeRead mba (toIndex exUb idx)
  writeM (MUnboxed exUb mba) idx elm = assert (inShape exUb idx) $ unsafeWrite mba (toIndex exUb idx) elm
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Shape sh, ExtShape sh, VUM.Unbox elm) => PrimArrayOps Unboxed sh elm where
  bounds (Unboxed exUb _) = (zeroDim,exUb `subDim` unitDim)
  freeze (MUnboxed exUb mba) = Unboxed exUb `liftM` unsafeFreeze mba
  index (Unboxed exUb ba) idx = assert (inShape exUb idx) $ unsafeIndex ba (toIndex exUb idx)
  {-# INLINE bounds #-}
  {-# INLINE freeze #-}
  {-# INLINE index #-}

instance (Shape sh, ExtShape sh, VUM.Unbox e, VUM.Unbox e') => PrimArrayMap Unboxed sh e e' where
  map f (Unboxed sh xs) = Unboxed sh (VU.map f xs)
  {-# INLINE map #-}



-- * Boxed, multidimensional arrays.

-- data Boxed sh e = Boxed !sh !(V.Vector e)


{-
 -
 - This stuff tells us how to write efficient generics on large data
 - constructors like the Turner and Vienna ctors.
 -

import qualified Data.Generics.TH as T

data Unboxed sh e = Unboxed !sh !(VU.Vector e)
  deriving (Show,Eq,Ord)

data X e = X (Unboxed DIM1 e) (Unboxed DIM1 e)
  deriving (Show,Eq,Ord)

x :: X Int
x = X z z where z = (Unboxed (Z:.10) (VU.fromList [ 0 .. 10] ))

pot :: X Int -> X Double
pot = $( T.thmapT (T.mkTs ['f]) [t| X Int |] ) where
  f :: Unboxed DIM1 Int -> Unboxed DIM1 Double
  f (Unboxed sh xs) = Unboxed sh (VU.map fromIntegral xs)

-}

