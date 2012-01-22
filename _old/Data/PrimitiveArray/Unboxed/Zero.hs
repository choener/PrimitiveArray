{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
--
-- TODO missing UNPACK due to http://hackage.haskell.org/trac/ghc/ticket/3990
-- (UNPACK doesn't unbox data families)

module Data.PrimitiveArray.Unboxed.Zero where

--import qualified Data.Vector.Unboxed.Mutable as VUM
--import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST
import Control.Monad
import Data.Array.Repa.Shape
import Control.Exception (assert)
import Data.Primitive.Types
import Data.Primitive

import Data.PrimitiveArray

import Data.Array.Repa.Index


instance (Prim elm, Shape sh, Show elm, Show sh) => PrimArrayOps sh elm where
  -- | An immutable PrimArray has a lower bound (lsh), and upper bound (ush)
  -- and an upper bound minus unitDim (ush'), returned by bounds
  data PrimArray sh elm = PrimArray0 !sh !sh !ByteArray
  unsafeIndex (PrimArray0 ush ush' v) idx = assert (inShapeRange zeroDim ush idx)
                                             $ indexByteArray v (toIndex ush idx)
  bounds (PrimArray0 ush ush' _) = (zeroDim,ush')
  inBounds (PrimArray0 ush ush' _) idx = inShapeRange zeroDim ush idx
  fromAssocs lsh ush' def xs = assert (lsh == zeroDim) $
    runST $ do
      vM <- fromAssocsM lsh ush' def xs
      unsafeFreezeM vM
  assocs (PrimArray0 ush ush' v) = map (\k -> (fromIndex ush k, indexByteArray v k))
                                 $ [0 .. toIndex ush ush']
  fromList lsh ush' xs = assert (lsh == zeroDim) $
    runST $ do
      vM <- fromListM lsh ush' xs
      unsafeFreezeM vM
  toList (PrimArray0 ush ush' v) = [indexByteArray v k | k <- [0 .. toIndex ush ush']]
  {-# INLINE unsafeIndex #-}
  {-# INLINE bounds #-}
  {-# INLINE inBounds #-}
  {-# INLINE fromAssocs #-}
  {-# INLINE fromList #-}
  {-# INLINE toList #-}

instance (Show elm, Show sh, Prim elm) => Show (PrimArray sh elm) where
  show _ = error "not implemented"

{-
deriving instance (Show elm, Show sh, Prim elm) => Show (PrimArray sh elm)

deriving instance (Read elm, Read sh, Prim elm) => Read (PrimArray sh elm)
-}

instance (Prim elm, Shape sh) => PrimArrayOpsM sh elm (ST s) where
  data PrimArrayM sh elm (ST s) = PrimArrayST0 !sh !sh !(MutableByteArray s)
  readM (PrimArrayST0 ush ush' v) sh = readByteArray v (toIndex ush sh)
  writeM (PrimArrayST0 ush suh' v) sh e = writeByteArray v (toIndex ush sh) e
  fromAssocsM lsh ush' def xs = do
    let ush = ush' `addDim` unitDim
    v <- newByteArray (size ush * sizeOf def)
    forM_ [0 .. toIndex ush ush'] $ \k -> writeByteArray v k def
    forM_ xs $ \(k,e) -> writeByteArray v (toIndex ush k) e
    return $ PrimArrayST0 ush ush' v
  unsafeFreezeM (PrimArrayST0 ush ush' v) = do
    v' <- unsafeFreezeByteArray v
    return $ PrimArray0 ush ush' v'
  boundsM (PrimArrayST0 ush ush' _) = (zeroDim,ush')
  inBoundsM (PrimArrayST0 ush ush' _) idx = inShapeRange zeroDim ush idx
  fromListM lsh ush' xs = do
    let ush = ush' `addDim` unitDim
    v <- newByteArray (size ush * sizeOf (undefined `asTypeOf` (head xs)))
    zipWithM_ (\k x -> writeByteArray v k x) [0..] xs
    return $ PrimArrayST0 ush ush' v
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}
  {-# INLINE fromListM #-}

instance (Prim elm, Shape sh) => PrimArrayOpsM sh elm IO where
  data PrimArrayM sh elm IO = PrimArrayIO0 !sh !sh !(MutableByteArray RealWorld)
  readM (PrimArrayIO0 ush ush' v) sh = readByteArray v (toIndex ush sh)
  writeM (PrimArrayIO0 ush suh' v) sh e = writeByteArray v (toIndex ush sh) e
  fromAssocsM lsh ush' def xs = do
    let ush = ush' `addDim` unitDim
    v <- newByteArray (size ush * sizeOf def)
    forM_ [0 .. toIndex ush ush'] $ \k -> writeByteArray v k def
    return $ PrimArrayIO0 ush ush' v
  unsafeFreezeM (PrimArrayIO0 ush ush' v) = do
    v' <- unsafeFreezeByteArray v
    return $ PrimArray0 ush ush' v'
  boundsM (PrimArrayIO0 ush ush' _) = (zeroDim,ush')
  inBoundsM (PrimArrayIO0 ush ush' _) idx = inShapeRange zeroDim ush idx
  fromListM lsh ush' xs = do
    let ush = ush' `addDim` unitDim
    v <- newByteArray (size ush * sizeOf (undefined `asTypeOf` (head xs)))
    zipWithM_ (\k x -> writeByteArray v k x) [0..] xs
    return $ PrimArrayIO0 ush ush' v
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}
  {-# INLINE fromListM #-}
