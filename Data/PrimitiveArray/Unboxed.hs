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
  data PrimArray sh elm = PrimArray sh sh (VU.Vector elm)
  unsafeIndex (PrimArray lsh ush v) idx = assert (inShapeRange lsh ush idx)
                                        $ v `VU.unsafeIndex` (toIndex ush idx - toIndex ush lsh)
  bounds (PrimArray lsh ush _) = (lsh,ush)
  inBounds (PrimArray lsh ush _) idx = inShapeRange lsh ush idx
  fromAssocs lsh ush def xs = PrimArray lsh ush
                            $ VU.replicate (size ush - size lsh) def
                            VU.// map (\(k,v) -> (toIndex ush k - toIndex ush lsh,v)) xs
  {-# INLINE unsafeIndex #-}
  {-# INLINE bounds #-}
  {-# INLINE inBounds #-}
  {-# INLINE fromAssocs #-}

deriving instance (Show elm, Show sh, VU.Unbox elm) => Show (PrimArray sh elm)

deriving instance (Read elm, Read sh, VU.Unbox elm) => Read (PrimArray sh elm)



instance (VUM.Unbox elm, Shape sh) => PrimArrayOpsM sh elm (ST s) where
  data PrimArrayM sh elm (ST s) = PrimArrayM sh sh (VUM.STVector s elm)
  readM (PrimArrayM lsh ush v) sh = VUM.unsafeRead v (toIndex ush sh - toIndex ush lsh)
  writeM (PrimArrayM lsh ush v) sh e = VUM.unsafeWrite v (toIndex ush sh - toIndex ush lsh) e
  fromAssocsM lsh ush def xs = do
    v <- VUM.new (size ush - size lsh)
    VUM.set v def
    forM_ xs $ \(k,e) -> assert (inShapeRange lsh ush k)
                      $ VUM.unsafeWrite v (toIndex ush k - toIndex ush lsh) e
    return $ PrimArrayM lsh ush v
  unsafeFreezeM (PrimArrayM lsh ush v) = do
    v' <- VU.unsafeFreeze v
    return $ PrimArray lsh ush v'
  boundsM (PrimArrayM lsh ush _) = (lsh,ush)
  inBoundsM (PrimArrayM lsh ush _) idx = inShapeRange lsh ush idx
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE fromAssocsM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE boundsM #-}
  {-# INLINE inBoundsM #-}

