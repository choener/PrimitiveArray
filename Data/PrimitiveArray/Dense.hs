
-- | Dense primitive arrays where the lower index is zero (or the
-- equivalent of zero for newtypes and enumerations).
--
-- Actual @write@s to data structures use a more safe @write@ instead of
-- the unsafe @unsafeWrite@. Writes also tend to occur much less in DP
-- algorithms (say, N^2 writes for an N^3 time algorithm -- mostly reads
-- are being executed).
--
-- TODO consider if we want to force the lower index to be zero, or allow
-- non-zero lower indices. Will have to be considered together with the
-- @Index.Class@ module!

module Data.PrimitiveArray.Dense where

import           Control.Exception (assert)
import           Control.Monad (liftM, forM_, zipWithM_)
import           Control.Monad.Primitive (PrimState)
import           Data.Aeson (ToJSON,FromJSON)
import           Data.Binary (Binary)
import           Data.Serialize (Serialize)
import           Data.Vector.Binary
import           Data.Vector.Cereal
import           Data.Vector.Generic.Mutable as GM hiding (length)
import           Data.Vector.Unboxed.Mutable (Unbox)
import           GHC.Generics (Generic)
import qualified Data.Vector as V hiding (forM_, length, zipWithM_)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as VU hiding (forM_, length, zipWithM_)


import           Data.PrimitiveArray.Class
import           Data.PrimitiveArray.Index



-- * Unboxed, multidimensional arrays.

data Unboxed sh e = Unboxed !sh !sh !(VU.Vector e)
  deriving (Read,Show,Eq,Generic)

instance (Binary    sh, Binary    e, Unbox e) => Binary    (Unboxed sh e)
instance (Serialize sh, Serialize e, Unbox e) => Serialize (Unboxed sh e)
instance (ToJSON    sh, ToJSON    e, Unbox e) => ToJSON    (Unboxed sh e)
instance (FromJSON  sh, FromJSON  e, Unbox e) => FromJSON  (Unboxed sh e)

data instance MutArr m (Unboxed sh e) = MUnboxed !sh !sh !(VU.MVector (PrimState m) e)

instance (Index sh, Unbox elm) => MPrimArrayOps Unboxed sh elm where
  boundsM (MUnboxed l h _) = (l,h)
  fromListM l h xs = do
    ma <- newM l h
    let (MUnboxed _ _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size l h) $ unsafeWrite mba k x) [0.. size l h -1] xs
    return ma
  newM l h = MUnboxed l h `liftM` new (size l h)
  newWithM l h def = do
    ma <- newM l h
    let (MUnboxed _ _ mba) = ma
    forM_ [0 .. size l h -1] $ \k -> unsafeWrite mba k def
    return ma
  readM  (MUnboxed l h mba) idx     = assert (inBounds l h idx) $ unsafeRead  mba (linearIndex l h idx)
  writeM (MUnboxed l h mba) idx elm = write mba (linearIndex l h idx) elm
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Index sh, Unbox elm) => PrimArrayOps Unboxed sh elm where
  bounds (Unboxed l h _) = (l,h)
  unsafeFreeze (MUnboxed l h mba) = Unboxed l h `liftM` G.unsafeFreeze mba
  unsafeThaw   (Unboxed  l h ba) = MUnboxed l h `liftM` G.unsafeThaw ba
  unsafeIndex  (Unboxed  l h ba) idx = {- assert (inShape exUb idx) $ -} G.unsafeIndex ba (linearIndex l h idx)
  transformShape tr (Unboxed l h ba) = Unboxed (tr l) (tr h) ba
  {-# INLINE bounds #-}
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw #-}
  {-# INLINE unsafeIndex #-}
  {-# INLINE transformShape #-}

instance (Index sh, Unbox e, Unbox e') => PrimArrayMap Unboxed sh e e' where
  map f (Unboxed l h xs) = Unboxed l h (VU.map f xs)
  {-# INLINE map #-}



-- * Boxed, multidimensional arrays.

data Boxed sh e = Boxed !sh !sh !(V.Vector e)
  deriving (Read,Show,Eq,Generic)

instance (Binary    sh, Binary    e)  => Binary    (Boxed sh e)
instance (Serialize sh, Serialize e)  => Serialize (Boxed sh e)
instance (ToJSON    sh, ToJSON    e)  => ToJSON    (Boxed sh e)
instance (FromJSON  sh, FromJSON  e)  => FromJSON  (Boxed sh e)

data instance MutArr m (Boxed sh e) = MBoxed !sh !sh !(V.MVector (PrimState m) e)

instance (Index sh) => MPrimArrayOps Boxed sh elm where
  boundsM (MBoxed l h _) = (l,h)
  fromListM l h xs = do
    ma <- newM l h
    let (MBoxed _ _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size l h) $ unsafeWrite mba k x) [0 .. size l h - 1] xs
    return ma
  newM l h =
    MBoxed l h `liftM` new (size l h)
  newWithM l h def = do
    ma <- newM l h
    let (MBoxed _ _ mba) = ma
    forM_ [0 .. size l h -1] $ \k -> unsafeWrite mba k def
    return ma
  readM  (MBoxed l h mba) idx     = assert (inBounds l h idx) $ GM.unsafeRead mba (linearIndex l h idx)
  writeM (MBoxed l h mba) idx elm = assert (inBounds l h idx) $ GM.write mba (linearIndex l h idx) elm
  {-# INLINE boundsM #-}
  {-# INLINE fromListM #-}
  {-# INLINE newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Index sh, Unbox elm) => PrimArrayOps Boxed sh elm where
  bounds (Boxed l h _) = (l,h)
  unsafeFreeze (MBoxed l h mba) = Boxed l h `liftM` G.unsafeFreeze mba
  unsafeThaw   (Boxed l h ba) = MBoxed l h `liftM` G.unsafeThaw ba
  unsafeIndex (Boxed l h ba) idx = {- assert (inShape exUb idx) $ -} G.unsafeIndex ba (linearIndex l h idx)
  transformShape tr (Boxed l h ba) = Boxed (tr l) (tr h) ba
  {-# INLINE bounds #-}
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw #-}
  {-# INLINE unsafeIndex #-}
  {-# INLINE transformShape #-}

instance (Index sh) => PrimArrayMap Boxed sh e e' where
  map f (Boxed l h xs) = Boxed l h (V.map f xs)
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

