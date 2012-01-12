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

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST
import Control.Monad
import Data.Array.Repa.Shape
import Control.Exception (assert)

import Data.PrimitiveArray

import Data.Array.Repa.Index



instance (VU.Unbox elm, Shape sh, Show elm, Show sh) => PrimArrayOps sh elm where
  -- | An immutable PrimArray has a lower bound (lsh), and upper bound (ush)
  -- and an upper bound minus unitDim (ush'), returned by bounds
  data PrimArray sh elm = PrimArray0 !sh !sh !(VU.Vector elm)
  unsafeIndex (PrimArray0 ush ush' v) idx = assert (inShapeRange zeroDim ush idx)
                                             $ v `VU.unsafeIndex` (toIndex ush idx)
  bounds (PrimArray0 ush ush' _) = (zeroDim,ush')
  inBounds (PrimArray0 ush ush' _) idx = inShapeRange zeroDim ush idx
  fromAssocs lsh ush' def xs = assert (lsh == zeroDim) $
    let ush = ush' `addDim` unitDim
    in  PrimArray0 ush ush'
        $ VU.replicate (size ush) def
        VU.// map (\(k,v) -> if (inShapeRange zeroDim ush k)
                             then (toIndex ush k,v)
                             else error $ show (ush,k,v)
                  ) xs
  assocs (PrimArray0 ush ush' v) = map (\(k,v) -> (fromIndex ush k, v))
                                    . VU.toList
                                    . VU.indexed
                                    $ v
  {-# INLINE unsafeIndex #-}
  {-# INLINE bounds #-}
  {-# INLINE inBounds #-}
  {-# INLINE fromAssocs #-}

deriving instance (Show elm, Show sh, VU.Unbox elm) => Show (PrimArray sh elm)

deriving instance (Read elm, Read sh, VU.Unbox elm) => Read (PrimArray sh elm)

instance (VUM.Unbox elm, Shape sh) => PrimArrayOpsM sh elm (ST s) where
  data PrimArrayM sh elm (ST s) = PrimArrayST0 !sh !sh !(VUM.STVector s elm)
  readM (PrimArrayST0 ush ush' v) sh = VUM.unsafeRead v (toIndex ush sh)
  writeM (PrimArrayST0 ush suh' v) sh e = VUM.unsafeWrite v (toIndex ush sh) e
  fromAssocsM lsh ush' def xs = do
    let ush = ush' `addDim` unitDim
    v <- VUM.new (size ush)
    VUM.set v def
    forM_ xs $ \(k,e) -> assert (inShapeRange zeroDim ush k)
                      $ VUM.unsafeWrite v (toIndex ush k) e
    return $ PrimArrayST0 ush ush' v
  unsafeFreezeM (PrimArrayST0 ush ush' v) = do
    v' <- VU.unsafeFreeze v
    return $ PrimArray0 ush ush' v'
  boundsM (PrimArrayST0 ush ush' _) = (zeroDim,ush')
  inBoundsM (PrimArrayST0 ush ush' _) idx = inShapeRange zeroDim ush idx
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}

instance (VUM.Unbox elm, Shape sh) => PrimArrayOpsM sh elm IO where
  data PrimArrayM sh elm IO = PrimArrayIO0 !sh !sh !(VUM.IOVector elm)
  readM (PrimArrayIO0 ush ush' v) sh = VUM.unsafeRead v (toIndex ush sh)
  writeM (PrimArrayIO0 ush suh' v) sh e = VUM.unsafeWrite v (toIndex ush sh) e
  fromAssocsM lsh ush' def xs = do
    let ush = ush' `addDim` unitDim
    v <- VUM.new (size ush)
    VUM.set v def
    forM_ xs $ \(k,e) -> assert (inShapeRange zeroDim ush k)
                      $ VUM.unsafeWrite v (toIndex ush k) e
    return $ PrimArrayIO0 ush ush' v
  unsafeFreezeM (PrimArrayIO0 ush ush' v) = do
    v' <- VU.unsafeFreeze v
    return $ PrimArray0 ush ush' v'
  boundsM (PrimArrayIO0 ush ush' _) = (zeroDim,ush')
  inBoundsM (PrimArrayIO0 ush ush' _) idx = inShapeRange zeroDim ush idx
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}
