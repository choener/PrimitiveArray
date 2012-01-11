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

import Data.Array.Repa.Index



instance (VU.Unbox elm, Shape sh, Show elm, Show sh) => PrimArrayOps sh elm where
  -- | An immutable PrimArray has a lower bound (lsh), and upper bound (ush)
  -- and an upper bound minus unitDim (ush'), returned by bounds
  data PrimArray sh elm = PrimArray sh sh sh (VU.Vector elm)
  unsafeIndex (PrimArray lsh ush ush' v) idx = assert (inShapeRange lsh ush idx)
                                             $ v `VU.unsafeIndex` (toIndex ush idx - toIndex ush lsh)
  bounds (PrimArray lsh ush ush' _) = (lsh,ush')
  inBounds (PrimArray lsh ush ush' _) idx = inShapeRange lsh ush idx
  fromAssocs lsh ush' def xs =
    let ush = ush' `addDim` unitDim
    in  PrimArray lsh ush ush'
        $ VU.replicate (size ush - size lsh) def
        VU.// map (\(k,v) -> if (inShapeRange lsh ush k)
                             then (toIndex ush k - toIndex ush lsh,v)
                             else error $ show (lsh,ush,k,v)
                  ) xs
  assocs (PrimArray lsh ush ush' v) = map (\(k,v) -> (fromIndex ush $ k + toIndex ush lsh, v))
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
  data PrimArrayM sh elm (ST s) = PrimArrayST sh sh sh (VUM.STVector s elm)
  readM (PrimArrayST lsh ush ush' v) sh = VUM.unsafeRead v (toIndex ush sh - toIndex ush lsh)
  writeM (PrimArrayST lsh ush suh' v) sh e = VUM.unsafeWrite v (toIndex ush sh - toIndex ush lsh) e
  fromAssocsM lsh ush' def xs = do
    let ush = ush' `addDim` unitDim
    v <- VUM.new (size ush - size lsh)
    VUM.set v def
    forM_ xs $ \(k,e) -> assert (inShapeRange lsh ush k)
                      $ VUM.unsafeWrite v (toIndex ush k - toIndex ush lsh) e
    return $ PrimArrayST lsh ush ush' v
  unsafeFreezeM (PrimArrayST lsh ush ush' v) = do
    v' <- VU.unsafeFreeze v
    return $ PrimArray lsh ush ush' v'
  boundsM (PrimArrayST lsh ush ush' _) = (lsh,ush')
  inBoundsM (PrimArrayST lsh ush ush' _) idx = inShapeRange lsh ush idx
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}

instance (VUM.Unbox elm, Shape sh) => PrimArrayOpsM sh elm IO where
  data PrimArrayM sh elm IO = PrimArrayIO sh sh sh (VUM.IOVector elm)
  readM (PrimArrayIO lsh ush ush' v) sh = VUM.unsafeRead v (toIndex ush sh - toIndex ush lsh)
  writeM (PrimArrayIO lsh ush suh' v) sh e = VUM.unsafeWrite v (toIndex ush sh - toIndex ush lsh) e
  fromAssocsM lsh ush' def xs = do
    let ush = ush' `addDim` unitDim
    v <- VUM.new (size ush - size lsh)
    VUM.set v def
    forM_ xs $ \(k,e) -> assert (inShapeRange lsh ush k)
                      $ VUM.unsafeWrite v (toIndex ush k - toIndex ush lsh) e
    return $ PrimArrayIO lsh ush ush' v
  unsafeFreezeM (PrimArrayIO lsh ush ush' v) = do
    v' <- VU.unsafeFreeze v
    return $ PrimArray lsh ush ush' v'
  boundsM (PrimArrayIO lsh ush ush' _) = (lsh,ush')
  inBoundsM (PrimArrayIO lsh ush ush' _) idx = inShapeRange lsh ush idx
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}

