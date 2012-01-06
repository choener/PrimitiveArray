{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- | Primitive arrays with a small set of operations. Modelled after repa
-- arrays and indexing.
--
-- Array indexing starts ALWAYS at 0. The extend of the array is the number
-- elements per dimension. A dimension with valid indices [0 .. n] (including 0
-- and n) needs an extends (n+1).
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
  bounds :: PrimArray sh elm -> sh
  inBounds :: PrimArray sh elm -> sh -> Bool
  fromAssocs :: sh -> elm -> [(sh,elm)] -> PrimArray sh elm

class (PrimMonad m, Shape sh) => PrimArrayOpsM sh elm m where
  data PrimArrayM sh elm m :: *
  readM :: PrimArrayM sh elm m -> sh -> m elm
  writeM :: PrimArrayM sh elm m -> sh -> elm -> m ()
  -- | Create a monadic array from a list of associations
  fromAssocsM :: sh -> elm -> [(sh,elm)] -> m (PrimArrayM sh elm m)
  unsafeFreezeM :: PrimArrayM sh elm m -> m (PrimArray sh elm)
  boundsM :: PrimArrayM sh elm m -> sh
  inBoundsM :: PrimArrayM sh elm m -> sh -> Bool



-- * Helper functions

(!) :: PrimArrayOps sh elm => PrimArray sh elm -> sh -> elm
(!) pa idx = assert (inBounds pa idx) $ unsafeIndex pa idx
{-# INLINE (!) #-}

