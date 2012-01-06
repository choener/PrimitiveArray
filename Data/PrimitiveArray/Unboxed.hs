{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.PrimitiveArray.Unboxed where

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST
import Control.Monad
import Data.Array.Repa.Shape
import Control.Exception (assert)

import Data.PrimitiveArray



instance (VU.Unbox elm, Shape sh) => PrimArrayOps sh elm where
  data PrimArray sh elm = PrimArray sh (VU.Vector elm)
  unsafeIndex (PrimArray sh v) idx = assert (inShapeRange zeroDim sh idx) $ v `VU.unsafeIndex` (toIndex sh sh)
  bounds (PrimArray sh _) = sh
  inBounds (PrimArray sh _) idx = inShapeRange zeroDim sh idx
  {-# INLINE unsafeIndex #-}
  {-# INLINE bounds #-}
  {-# INLINE inBounds #-}

deriving instance (Show elm, Show sh, VU.Unbox elm) => Show (PrimArray sh elm)

deriving instance (Read elm, Read sh, VU.Unbox elm) => Read (PrimArray sh elm)



instance (VUM.Unbox elm, Shape sh) => PrimArrayOpsM sh elm (ST s) where
  data PrimArrayM sh elm (ST s) = PrimArrayM sh (VUM.STVector s elm)
  readM (PrimArrayM xtnd v) sh = VUM.unsafeRead v (toIndex xtnd sh)
  writeM (PrimArrayM xtnd v) sh e = VUM.unsafeWrite v (toIndex xtnd sh) e
  fromAssocsM xtnd def xs = do
    v <- VUM.new (size xtnd)
    VUM.set v def
    forM_ xs $ \(k,e) -> assert (inShapeRange zeroDim xtnd k) $ VUM.unsafeWrite v (toIndex xtnd k) e
    return $ PrimArrayM xtnd v
  unsafeFreezeM (PrimArrayM xtnd v) = do
    v' <- VU.unsafeFreeze v
    return $ PrimArray xtnd v'
  boundsM (PrimArrayM xtnd _) = xtnd
  inBoundsM (PrimArrayM xtnd _) idx = inShapeRange zeroDim xtnd idx
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}

