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

arr = fromAssocs lb ub def []
lb = Z:.0:.0 :: DIM2
ub = Z:.2:.2 :: DIM2
def = 0 :: Int



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
  data PrimArrayM sh elm (ST s) = PrimArrayM sh sh sh (VUM.STVector s elm)
  readM (PrimArrayM lsh ush ush' v) sh = VUM.unsafeRead v (toIndex ush sh - toIndex ush lsh)
  writeM (PrimArrayM lsh ush suh' v) sh e = VUM.unsafeWrite v (toIndex ush sh - toIndex ush lsh) e
  fromAssocsM lsh ush' def xs = do
    let ush = ush' `addDim` unitDim
    v <- VUM.new (size ush - size lsh)
    VUM.set v def
    forM_ xs $ \(k,e) -> assert (inShapeRange lsh ush k)
                      $ VUM.unsafeWrite v (toIndex ush k - toIndex ush lsh) e
    return $ PrimArrayM lsh ush ush' v
  unsafeFreezeM (PrimArrayM lsh ush ush' v) = do
    v' <- VU.unsafeFreeze v
    return $ PrimArray lsh ush ush' v'
  boundsM (PrimArrayM lsh ush ush' _) = (lsh,ush')
  inBoundsM (PrimArrayM lsh ush ush' _) idx = inShapeRange lsh ush idx
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}

