{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

-- | Minimal implementation of primitive / unboxed arrays. We can
-- create/read/write mutable arrays. Freeze mutable arrays into immutable ones,
-- and index immutable arrays. Tuples of primitive elements are supported as
-- well.
--
-- We use these arrays as base arrays for unboxed arrays.
--
-- Basically, this is a scaled-down version of unboxed vectors.
--
-- Currently allowed types: Int, Word, Double, Float, 2-tuples (pairs),
-- 3-tuples.

module Data.PrimitiveArray.Internal.Unboxed where

import Data.Primitive
import Data.Primitive.Types
import Control.Monad.Primitive
import Control.Monad
import Control.Exception (assert)
import Prelude hiding (read)
import GHC.Word



-- * Basic mutable arrays with associated data type.

-- | Associated mutable array type.

data family BMA s elm :: *



class BMAops elm where

  -- | Create a new mutable array with the given number of elements (elements,
  -- not bytes!).

  new   :: (Monad m, PrimMonad m) => Int -> m (BMA (PrimState m) elm)

  -- | Unsafe read operation. No bounds checking (except assert)!

  read  :: (Monad m, PrimMonad m) => BMA (PrimState m) elm -> Int -> m elm

  -- | Unsafe write operation. No bounds checking (except assert)!

  write :: (Monad m, PrimMonad m) => BMA (PrimState m) elm -> Int -> elm -> m ()



-- * Basic immutable arrays with associated data type.

-- | Associated immutable array type.

data family BA elm :: *



class BAops elm where

  -- | No-copy freeze operations from mutable to immutable array.

  freeze :: (Monad m, PrimMonad m) => BMA (PrimState m) elm -> m (BA elm)

  -- | Index into an immutable array.

  index  :: BA elm -> Int -> elm



-- * Instances for primitive types.

#define mkBMA(ty,con) \
instance BMAops ty where { \
  {-# INLINE new #-} \
; {-# INLINE read #-} \
; {-# INLINE write #-} \
; new n = con `liftM` newByteArray (n * sizeOf (undefined :: ty)) \
; read (con ma) k = assert (k < sizeofMutableByteArray ma) $ readByteArray ma k \
; write (con ma) k e = assert (k < sizeofMutableByteArray ma) $ writeByteArray ma k e }

-- ; data BMA s ty = con {-# UNPACK #-} !(MutableByteArray s) \

#define mkBA(ty,mcon,con) \
instance BAops ty where { \
  {-# INLINE freeze #-} \
; {-# INLINE index #-} \
; freeze (mcon mba) = con `liftM` unsafeFreezeByteArray mba \
; index (con ba) k = indexByteArray ba k }

-- ; data BA ty = con {-# UNPACK #-} !(ByteArray) \

newtype instance BMA s Int = BMAint (MutableByteArray s)
newtype instance BA    Int = BAint  (ByteArray)
mkBMA(Int,BMAint)
mkBA(Int,BMAint,BAint)

newtype instance BMA s Double = BMAdouble (MutableByteArray s)
newtype instance BA    Double = BAdouble  (ByteArray)
mkBMA(Double,BMAdouble)
mkBA(Double,BMAdouble,BAdouble)

{-
mkBMA(Float,BMAfloat)
mkBA(Float,BMAfloat,BAfloat)

mkBMA(Word,BMAword)
mkBA(Word,BMAword,BAword)
-}


newtype X = X Int

deriving instance BMAops X
deriving instance BAops  X


{-
instance BMAops Int where
  data BMA s Int = BMAInt {-# UNPACK #-} !(MutableByteArray s)
  new n = BMAInt `liftM` newByteArray (n * sizeOf (undefined :: Int))
  read (BMAInt ma) k = assert (k < sizeofMutableByteArray ma) $ readByteArray ma k
  write (BMAInt ma) k e = assert (k < sizeofMutableByteArray ma) $ writeByteArray ma k e
  {-# INLINE new #-}
  {-# INLINE read #-}
  {-# INLINE write #-}

instance BAops Int where
  data BA Int = BAInt {-# UNPACK #-} !(ByteArray)
  freeze (BMAInt mba) = BAInt `liftM` unsafeFreezeByteArray mba
  index (BAInt ba) k = indexByteArray ba k
  {-# INLINE freeze #-}
  {-# INLINE index #-}
-}



-- * Instances for tuples of primitive types.

data instance BMA s (e1,e2) = BMAt2 (BMA s e1) (BMA s e2)

instance (Prim e1, Prim e2, BMAops e1, BMAops e2) => BMAops (e1,e2) where
--  data BMA s (e1,e2) = BMAt2 {-# UNPACK #-} !(BMA s e1) {-# UNPACK #-} !(BMA s e2)
  new n = BMAt2 `liftM` new n `ap` new n
  read (BMAt2 ma1 ma2) k = (,) `liftM` read ma1 k `ap` read ma2 k
  write (BMAt2 ma1 ma2) k (e1,e2) = write ma1 k e1 >> write ma2 k e2
  {-# INLINE new #-}
  {-# INLINE read #-}
  {-# INLINE write #-}

data instance BA (e1,e2) = BAt2 (BA e1) (BA e2)

instance (Prim e1, Prim e2, BAops e1, BAops e2) => BAops (e1,e2) where
--  data BA (e1,e2) = BAt2 {-# UNPACK #-} !(BA e1) {-# UNPACK #-} !(BA e2)
  freeze (BMAt2 ma1 ma2) = BAt2 `liftM` freeze ma1 `ap` freeze ma2
  index (BAt2 ba1 ba2) k = (index ba1 k, index ba2 k)
  {-# INLINE freeze #-}
  {-# INLINE index #-}



{-
instance (Prim e1, Prim e2, Prim e3, BMAops e1, BMAops e2, BMAops e3) => BMAops (e1,e2,e3) where
  data BMA s (e1,e2,e3) = BMAt3 {-# UNPACK #-} !(BMA s e1) {-# UNPACK #-} !(BMA s e2) {-# UNPACK #-} !(BMA s e3)
  new n = BMAt3 `liftM` new n `ap` new n `ap` new n
  read (BMAt3 ma1 ma2 ma3) k = (,,) `liftM` read ma1 k `ap` read ma2 k `ap` read ma3 k
  write (BMAt3 ma1 ma2 ma3) k (e1,e2,e3) = write ma1 k e1 >> write ma2 k e2 >> write ma3 k e3
  {-# INLINE new #-}
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (Prim e1, Prim e2, Prim e3, BAops e1, BAops e2, BAops e3) => BAops (e1,e2,e3) where
  data BA (e1,e2,e3) = BAt3 {-# UNPACK #-} !(BA e1) {-# UNPACK #-} !(BA e2) {-# UNPACK #-} !(BA e3)
  freeze (BMAt3 ma1 ma2 ma3) = BAt3 `liftM` freeze ma1 `ap` freeze ma2 `ap` freeze ma3
  index (BAt3 ba1 ba2 ba3) k = (index ba1 k, index ba2 k, index ba3 k)
  {-# INLINE freeze #-}
  {-# INLINE index #-}
-}
