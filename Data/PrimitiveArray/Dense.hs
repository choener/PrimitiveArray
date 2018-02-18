
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
--
-- TODO while @Unboxed@ is, in princile, @Hashable@, we'd need the
-- corresponding @VU.Vector@ instances ...

module Data.PrimitiveArray.Dense where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad (liftM, forM_, zipWithM_)
import           Control.Monad.Primitive (PrimState)
import           Data.Aeson (ToJSON,FromJSON)
import           Data.Binary (Binary)
import           Data.Serialize (Serialize)
import           Data.Vector.Binary
import           Data.Vector.Serialize
import           Data.Vector.Generic.Mutable as GM hiding (length)
import           Data.Vector.Unboxed.Mutable (Unbox)
import           GHC.Generics (Generic)
import qualified Data.Vector as V hiding (forM_, length, zipWithM_)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as VU hiding (forM_, length, zipWithM_)
import           Data.Hashable (Hashable)
import           Data.Typeable (Typeable)


import           Data.PrimitiveArray.Class
import           Data.PrimitiveArray.Index.Class



-- * Unboxed, multidimensional arrays.

data Unboxed sh e = Unboxed !(LimitType sh) !(VU.Vector e)
--  deriving (Read,Show,Eq,Generic,Typeable)

deriving instance (Eq      (LimitType sh), Eq e     , Unbox e) ⇒ Eq      (Unboxed sh e)
deriving instance (Generic (LimitType sh), Generic e, Unbox e) ⇒ Generic (Unboxed sh e)
deriving instance (Read    (LimitType sh), Read e   , Unbox e) ⇒ Read    (Unboxed sh e)
deriving instance (Show    (LimitType sh), Show e   , Unbox e) ⇒ Show    (Unboxed sh e)

instance (Binary    (LimitType sh), Binary    e, Unbox e, Generic (LimitType sh), Generic e) => Binary    (Unboxed sh e)
instance (Serialize (LimitType sh), Serialize e, Unbox e, Generic (LimitType sh), Generic e) => Serialize (Unboxed sh e)
instance (ToJSON    (LimitType sh), ToJSON    e, Unbox e, Generic (LimitType sh), Generic e) => ToJSON    (Unboxed sh e)
instance (FromJSON  (LimitType sh), FromJSON  e, Unbox e, Generic (LimitType sh), Generic e) => FromJSON  (Unboxed sh e)
instance (Hashable  (LimitType sh), Hashable  e, Hashable (VU.Vector e), Unbox e, Generic (LimitType sh), Generic e) => Hashable  (Unboxed sh e)

instance (NFData (LimitType sh)) => NFData (Unboxed sh e) where
  rnf (Unboxed h xs) = rnf h `seq` rnf xs
  {-# Inline rnf #-}

data instance MutArr m (Unboxed sh e) = MUnboxed !(LimitType sh) !(VU.MVector (PrimState m) e)
  deriving (Generic,Typeable)

instance (NFData (LimitType sh)) => NFData (MutArr m (Unboxed sh e)) where
  rnf (MUnboxed h xs) = rnf h `seq` rnf xs
  {-# Inline rnf #-}

instance (Index sh, Unbox elm) => MPrimArrayOps Unboxed sh elm where
  upperBoundM (MUnboxed h _) = h
  fromListM h xs = do
    ma <- newM h
    let (MUnboxed _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size h) $ unsafeWrite mba k x) [0.. size h -1] xs
    return ma
  newM h = MUnboxed h `liftM` new (size h)
  newWithM h def = do
    ma <- newM h
    let (MUnboxed _ mba) = ma
    forM_ [0 .. size h -1] $ \k -> unsafeWrite mba k def
    return ma
  readM  (MUnboxed h mba) idx     = assert (inBounds h idx) $ unsafeRead  mba (linearIndex h idx)
  writeM (MUnboxed h mba) idx elm = assert (inBounds h idx) $ unsafeWrite mba (linearIndex h idx) elm
  {-# INLINE upperBoundM #-}
  {-# INLINE fromListM #-}
  {-# NoInline newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Index sh, Unbox elm) => PrimArrayOps Unboxed sh elm where
  upperBound (Unboxed h _) = h
  unsafeFreeze (MUnboxed h mba) = Unboxed h `liftM` G.unsafeFreeze mba
  unsafeThaw   (Unboxed  h ba) = MUnboxed h `liftM` G.unsafeThaw ba
  unsafeIndex  (Unboxed  h ba) idx = G.unsafeIndex ba (linearIndex h idx)
  transformShape tr (Unboxed h ba) = Unboxed (tr h) ba
  {-# INLINE upperBound #-}
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw #-}
  {-# INLINE unsafeIndex #-}
  {-# INLINE transformShape #-}

instance (Index sh, Unbox e, Unbox e') => PrimArrayMap Unboxed sh e e' where
  map f (Unboxed h xs) = Unboxed h (VU.map f xs)
  {-# INLINE map #-}



-- * Boxed, multidimensional arrays.

data Boxed sh e = Boxed !(LimitType sh) !(V.Vector e)

deriving instance (Read    (LimitType sh), Read e) ⇒ Read (Boxed sh e)
deriving instance (Show    (LimitType sh), Show e) ⇒ Show (Boxed sh e)
deriving instance (Eq      (LimitType sh), Eq   e) ⇒ Eq   (Boxed sh e)
deriving instance (Generic (LimitType sh), Generic e) ⇒ Generic (Boxed sh e)

instance (Binary    (LimitType sh), Binary    e, Unbox e, Generic (LimitType sh), Generic e) => Binary    (Boxed sh e)
instance (Serialize (LimitType sh), Serialize e, Unbox e, Generic (LimitType sh), Generic e) => Serialize (Boxed sh e)
instance (ToJSON    (LimitType sh), ToJSON    e, Unbox e, Generic (LimitType sh), Generic e) => ToJSON    (Boxed sh e)
instance (FromJSON  (LimitType sh), FromJSON  e, Unbox e, Generic (LimitType sh), Generic e) => FromJSON  (Boxed sh e)
instance (Hashable  (LimitType sh), Hashable  e, Hashable (V.Vector e), Unbox e, Generic (LimitType sh), Generic e) => Hashable  (Boxed sh e)

instance (NFData (LimitType sh), NFData e) => NFData (Boxed sh e) where
  rnf (Boxed h xs) = rnf h `seq` rnf xs
  {-# Inline rnf #-}

data instance MutArr m (Boxed sh e) = MBoxed !(LimitType sh) !(V.MVector (PrimState m) e)
  deriving (Generic,Typeable)

instance (NFData (LimitType sh)) => NFData (MutArr m (Boxed sh e)) where
  rnf (MBoxed h xs) = rnf h -- no rnf for the data !
  {-# Inline rnf #-}

instance (Index sh) => MPrimArrayOps Boxed sh elm where
  upperBoundM (MBoxed h _) = h
  fromListM h xs = do
    ma <- newM h
    let (MBoxed _ mba) = ma
    zipWithM_ (\k x -> assert (length xs == size h) $ unsafeWrite mba k x) [0 .. size h - 1] xs
    return ma
  newM h =
    MBoxed h `liftM` new (size h)
  newWithM h def = do
    ma <- newM h
    let (MBoxed _ mba) = ma
    forM_ [0 .. size h -1] $ \k -> unsafeWrite mba k def
    return ma
  readM  (MBoxed h mba) idx     = assert (inBounds h idx) $ GM.unsafeRead  mba (linearIndex h idx)
  writeM (MBoxed h mba) idx elm = assert (inBounds h idx) $ GM.unsafeWrite mba (linearIndex h idx) elm
  {-# INLINE upperBoundM #-}
  {-# INLINE fromListM #-}
  {-# NoInline newM #-}
  {-# INLINE newWithM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}

instance (Index sh) => PrimArrayOps Boxed sh elm where
  upperBound (Boxed h _) = h
  unsafeFreeze (MBoxed h mba) = Boxed h `liftM` G.unsafeFreeze mba
  unsafeThaw   (Boxed h ba) = MBoxed h `liftM` G.unsafeThaw ba
  unsafeIndex (Boxed h ba) idx = assert (inBounds h idx) $ G.unsafeIndex ba (linearIndex h idx)
  transformShape tr (Boxed h ba) = Boxed (tr h) ba
  {-# INLINE upperBound #-}
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw #-}
  {-# INLINE unsafeIndex #-}
  {-# INLINE transformShape #-}

instance (Index sh) => PrimArrayMap Boxed sh e e' where
  map f (Boxed h xs) = Boxed h (V.map f xs)
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

