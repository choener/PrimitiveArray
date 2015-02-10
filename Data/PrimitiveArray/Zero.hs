
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Primitive arrays where the lower index is zero (or the equivalent of zero
-- for newtypes and enumerations).

module Data.PrimitiveArray.Zero where

import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Primitive (PrimState)
import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Generic as G hiding (forM_, length, zipWithM_, new, unsafeFreeze, unsafeIndex, unsafeThaw)
import qualified Data.Vector.Generic as G
import           Data.Vector.Generic.Mutable as GM hiding (length)
import           GHC.Generics
import qualified Data.Vector as V hiding (forM_, length, zipWithM_)
import qualified Data.Vector.Unboxed as VU hiding (forM_, length, zipWithM_)
import qualified Data.Vector.Unboxed.Mutable as VUM hiding (length)
import           Data.Vector.Binary
import           Data.Vector.Cereal

-- import           Data.Array.Repa.Index
-- import           Data.Array.Repa.Shape

-- import           Data.Array.Repa.ExtShape

import           Data.PrimitiveArray.Class
import           Data.PrimitiveArray.Index



-- * Unboxed, multidimensional arrays.

data Unboxed sh e = Unboxed !sh !(VU.Vector e)
  deriving (Read,Show,Eq,Generic)

instance (Binary sh, Binary e, VUM.Unbox e) => Binary (Unboxed sh e)
instance (Serialize sh, Serialize e, VUM.Unbox e) => Serialize (Unboxed sh e)
instance (ToJSON sh, ToJSON e, VUM.Unbox e) => ToJSON (Unboxed sh e)
instance (FromJSON sh, FromJSON e, VUM.Unbox e) => FromJSON (Unboxed sh e)

data instance MutArr m (Unboxed sh e) = MUnboxed !sh !(VU.MVector (PrimState m) e)

instance (Index sh, VUM.Unbox elm) => MPrimArrayOps Unboxed sh elm where
  boundsM (MUnboxed exUb _) = (zeroDim,exUb `subDim` unitDim)
  fromListM inLb inUb xs = do
    ma <- newM inLb inUb
    let exUb = inUb `addDim` unitDim
    let (MUnboxed _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size exUb) $ unsafeWrite mba k x) [0.. size exUb -1] xs
    return ma
  newM inLb inUb = let exUb = inUb `addDim` unitDim in
    unless (inLb == zeroDim) (error "MArr0 lb/=zeroDim") >>
    MUnboxed exUb `liftM` new (size exUb)
  newWithM inLb inUb def = do
    let exUb = inUb `addDim` unitDim
    ma <- newM inLb inUb
    let (MUnboxed _ mba) = ma
    forM_ [0 .. size exUb -1] $ \k -> unsafeWrite mba k def
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
  unsafeFreeze (MUnboxed exUb mba) = Unboxed exUb `liftM` G.unsafeFreeze mba
  unsafeThaw   (Unboxed exUb ba) = MUnboxed exUb `liftM` G.unsafeThaw ba
  unsafeIndex  (Unboxed exUb ba) idx = assert (inShape exUb idx) $ G.unsafeIndex ba (toIndex exUb idx)
  transformShape tr (Unboxed exUb ba) = Unboxed (tr exUb) ba
  {-# INLINE bounds #-}
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw #-}
  {-# INLINE unsafeIndex #-}
  {-# INLINE transformShape #-}

instance (Shape sh, ExtShape sh, VUM.Unbox e, VUM.Unbox e') => PrimArrayMap Unboxed sh e e' where
  map f (Unboxed sh xs) = Unboxed sh (VU.map f xs)
  {-# INLINE map #-}



-- * Boxed, multidimensional arrays.

data Boxed sh e = Boxed !sh !(V.Vector e)
  deriving (Read,Show,Eq,Generic)

instance (Binary sh, Binary e) => Binary (Boxed sh e)
instance (Serialize sh, Serialize e) => Serialize (Boxed sh e)
instance (ToJSON sh, ToJSON e) => ToJSON (Boxed sh e)
instance (FromJSON sh, FromJSON e) => FromJSON (Boxed sh e)

data instance MutArr m (Boxed sh e) = MBoxed !sh !(V.MVector (PrimState m) e)

instance (Shape sh, ExtShape sh, VUM.Unbox elm) => MPrimArrayOps Boxed sh elm where
  boundsM (MBoxed exUb _) = (zeroDim,exUb `subDim` unitDim)
  fromListM inLb inUb xs = do
    ma <- newM inLb inUb
    let exUb = inUb `addDim` unitDim
    let (MBoxed _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size exUb) $ unsafeWrite mba k x) [0 .. size exUb - 1] xs -- [0.. toIndex exUb inUb] xs
    return ma
  newM inLb inUb = let exUb = inUb `addDim` unitDim in
    unless (inLb == zeroDim) (error "MArr0 lb/=zeroDim") >>
    MBoxed exUb `liftM` new (size exUb)
  newWithM inLb inUb def = do
    let exUb = inUb `addDim` unitDim
    ma <- newM inLb inUb
    let (MBoxed _ mba) = ma
    forM_ [0 .. size exUb -1] $ \k -> unsafeWrite mba k def
    return ma
  readM (MBoxed exUb mba) idx = assert (inShape exUb idx) $ unsafeRead mba (toIndex exUb idx)
  writeM (MBoxed exUb mba) idx elm = assert (inShape exUb idx) $ GM.unsafeWrite mba (toIndex exUb idx) elm
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Shape sh, ExtShape sh, VUM.Unbox elm) => PrimArrayOps Boxed sh elm where
  bounds (Boxed exUb _) = (zeroDim,exUb `subDim` unitDim)
  unsafeFreeze (MBoxed exUb mba) = Boxed exUb `liftM` G.unsafeFreeze mba
  unsafeThaw   (Boxed exUb ba) = MBoxed exUb `liftM` G.unsafeThaw ba
  unsafeIndex (Boxed exUb ba) idx = assert (inShape exUb idx) $ G.unsafeIndex ba (toIndex exUb idx)
  transformShape tr (Boxed exUb ba) = Boxed (tr exUb) ba
  {-# INLINE bounds #-}
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw #-}
  {-# INLINE unsafeIndex #-}
  {-# INLINE transformShape #-}

instance (Shape sh, ExtShape sh) => PrimArrayMap Boxed sh e e' where
  map f (Boxed sh xs) = Boxed sh (V.map f xs)
  {-# INLINE map #-}



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

