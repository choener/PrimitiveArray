{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- | Primitive arrays with a small set of operations. Modelled after repa
-- arrays and indexing.
--
-- Array indexing is between [i..j] per dimension.
--
-- All operations are UNSAFE. In interpreted code, "assert" provides a safety
-- net.

module Data.PrimitiveArray where

import Control.Monad.Primitive (PrimMonad)
import Data.Array.Repa.Shape (Shape)
import Control.Exception (assert)





class Shape sh => PrimArrayOps sh elm where
  data PrimArray sh elm :: *
  unsafeIndex :: PrimArray sh elm -> sh -> elm
  bounds :: PrimArray sh elm -> (sh,sh)
  inBounds :: PrimArray sh elm -> sh -> Bool
  fromAssocs :: sh -> sh -> elm -> [(sh,elm)] -> PrimArray sh elm
  assocs :: PrimArray sh elm -> [(sh,elm)]
  fromList :: sh -> sh -> [elm] -> PrimArray sh elm
  toList :: PrimArray sh elm -> [elm]

class (PrimMonad m, Shape sh) => PrimArrayOpsM sh elm m where
  data PrimArrayM sh elm m :: *
  readM :: PrimArrayM sh elm m -> sh -> m elm
  writeM :: PrimArrayM sh elm m -> sh -> elm -> m ()
  -- | Create a monadic array from a list of associations
  fromAssocsM :: sh -> sh -> elm -> [(sh,elm)] -> m (PrimArrayM sh elm m)
  unsafeFreezeM :: PrimArrayM sh elm m -> m (PrimArray sh elm)
  boundsM :: PrimArrayM sh elm m -> (sh,sh)
  inBoundsM :: PrimArrayM sh elm m -> sh -> Bool
  fromListM :: sh -> sh -> [elm] -> m (PrimArrayM sh elm m)

instance (Shape sh, Eq elm, PrimArrayOps sh elm) => Eq (PrimArray sh elm) where
  arr == brr = bounds arr == bounds brr && (and $ zipWith (==) (toList arr) (toList brr))

-- * Helper functions

(!) :: PrimArrayOps sh elm => PrimArray sh elm -> sh -> elm
(!) pa idx = assert (inBounds pa idx) $ unsafeIndex pa idx
{-# INLINE (!) #-}

