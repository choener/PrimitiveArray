
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
--
-- TODO rename to Dense.Vector, since there are other possibilities to store,
-- without basing on vector.

module Data.PrimitiveArray.Dense where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad (liftM, forM_, zipWithM_, when)
import           Control.Monad.Primitive (PrimState)
import           Data.Aeson (ToJSON,FromJSON)
import           Data.Binary (Binary)
import           Data.Data
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize)
import           Data.Typeable (Typeable)
import           Data.Vector.Binary
import           Data.Vector.Generic.Mutable as GM hiding (length)
import           Data.Vector.Serialize
import           Debug.Trace
import           GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import           Data.PrimitiveArray.Class
import           Data.PrimitiveArray.Index.Class



data Dense v sh e = Dense !(LimitType sh) !(v e)

type Unboxed sh e = Dense VU.Vector sh e

type Storable sh e = Dense VS.Vector sh e

type Boxed sh e = Dense V.Vector sh e



deriving instance (Eq      (LimitType sh), Eq (v e)     ) ⇒ Eq      (Dense v sh e)
deriving instance (Generic (LimitType sh), Generic (v e)) ⇒ Generic (Dense v sh e)
deriving instance (Read    (LimitType sh), Read (v e)   ) ⇒ Read    (Dense v sh e)
deriving instance (Show    (LimitType sh), Show (v e)   ) ⇒ Show    (Dense v sh e)

deriving instance Typeable (Dense v sh e)

deriving instance (Data (v e), Data (LimitType sh), Data e, Data sh, Typeable sh, Typeable e, Typeable v) ⇒ Data (Dense v sh e)

instance (Binary    (LimitType sh), Binary    (v e), Generic (LimitType sh), Generic (v e)) => Binary    (Dense v sh e)
instance (Serialize (LimitType sh), Serialize (v e), Generic (LimitType sh), Generic (v e)) => Serialize (Dense v sh e)
instance (ToJSON    (LimitType sh), ToJSON    (v e), Generic (LimitType sh), Generic (v e)) => ToJSON    (Dense v sh e)
instance (FromJSON  (LimitType sh), FromJSON  (v e), Generic (LimitType sh), Generic (v e)) => FromJSON  (Dense v sh e)
instance (Hashable  (LimitType sh), Hashable  (v e), Generic (LimitType sh), Generic (v e)) => Hashable  (Dense v sh e)

instance (NFData (LimitType sh), NFData (v e)) ⇒ NFData (Dense v sh e) where
  rnf (Dense h xs) = rnf h `seq` rnf xs
  {-# Inline rnf #-}



data instance MutArr m (Dense v sh e) = MDense !(LimitType sh) !(VG.Mutable v (PrimState m) e)
  deriving (Generic,Typeable)

instance (Show (LimitType sh), Show (VG.Mutable v (PrimState m) e), VG.Mutable v (PrimState m) e ~ mv) ⇒ Show (MutArr m (Dense v sh e)) where
  show (MDense sh mv) = show (sh,mv)

instance (NFData (LimitType sh), NFData (VG.Mutable v (PrimState m) e), VG.Mutable v (PrimState m) e ~ mv) ⇒ NFData (MutArr m (Dense v sh e)) where
  rnf (MDense h xs) = rnf h `seq` rnf xs
  {-# Inline rnf #-}

{-
instance
  ( Index sh, MutArr m (Dense v sh e) ~ mv
  , GM.MVector (VG.Mutable v) e
#if ADPFUSION_DEBUGOUTPUT
  , Show sh, Show (LimitType sh), Show e
#endif
  ) ⇒ MPrimArrayOps (Dense v) sh e where
-}

instance
  ( Index sh, VG.Vector v e
#if ADPFUSION_DEBUGOUTPUT
  , Show sh, Show (LimitType sh), Show e
#endif
  ) ⇒ PrimArrayOps (Dense v) sh e where

  -- ** pure operations

  {-# Inline upperBound #-}
  upperBound (Dense h _) = h
  {-# Inline unsafeFreezeM #-}
  unsafeFreezeM (MDense h mba) = Dense h `liftM` VG.unsafeFreeze mba
  {-# Inline unsafeThawM #-}
  unsafeThawM   (Dense h ba) = MDense h `liftM` VG.unsafeThaw ba
  {-# Inline unsafeIndex #-}
  unsafeIndex  (Dense h ba) idx = VG.unsafeIndex ba (linearIndex h idx)
  {-# Inline safeIndex #-}
  safeIndex (Dense h ba) idx = if inBounds h idx then Just $ unsafeIndex (Dense h ba) idx else Nothing
  {-# Inline transformShape #-}
  transformShape tr (Dense h ba) = Dense (tr h) ba
  --{-# Inline mapArray #-}
  --mapArray :: (VG.Vector v i, PrimArrayOps (Dense v) sh i) => (e -> i) -> Dense v sh e -> Dense v sh i
  --mapArray f (Dense h xs) = Dense h (VG.map f xs)

  -- ** monadic operations

  {-# Inline upperBoundM #-}
  upperBoundM (MDense h _) = h
  {-# Inline fromListM #-}
  fromListM h xs = do
    ma ← newM h
    let (MDense _ mba) = ma
    -- there need to be at least as many elements, as we want to fill. There could be more, in debug
    -- tests, we like to do @[0..]@ and this should not trigger the assert.
    SM.zipWithM_ (\k x → assert (length (Prelude.take (size h) xs) == size h) $ unsafeWrite mba k x) (SM.enumFromTo 0 (size h -1)) (SM.fromList xs)
    return ma
  {-# Inline newM #-}     -- TODO was NoInline, check if anything breaks!
  newM h = MDense h `liftM` new (size h)
  {-# Inline newSM #-}
  newSM = error "not implemented, use newM for dense arrays"
  {-# Inline newWithM #-}
  newWithM h def = do
    ma ← newM h
    let (MDense _ mba) = ma
    GM.set mba def
    return ma
  {-# Inline newWithSM #-}
  newWithSM = error "not implemented, use newWithSM for dense arrays"
  {-# Inline readM #-}
  readM  (MDense h mba) idx     = assert (inBounds h idx) $ unsafeRead  mba (linearIndex h idx)
  {-# Inline safeReadM #-}
  safeReadM dense idx = if inBoundsM dense idx then Just <$> readM dense idx else undefined
  {-# Inline writeM #-}
  writeM (MDense h mba) idx elm =
#if ADPFUSION_DEBUGOUTPUT
    (if inBounds h idx then id else traceShow ("writeM", h, idx, elm, size h, linearIndex h idx, inBounds h idx))
#endif
    assert (inBounds h idx) $ unsafeWrite mba (linearIndex h idx) elm
  {-# Inline safeWriteM #-}
  safeWriteM dense idx elm = when (inBoundsM dense idx) $ writeM dense idx elm

-- instance (Index sh, VG.Vector v e, VG.Vector v e') ⇒ PrimArrayMap (Dense v) sh e e' where



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

