
-- | This module exports everything that @Data.PrimitiveArray@ exports, but
-- it will do some bounds-checking on certain operations.
--
-- Checked are: @(!)@

module Data.PrimitiveArray.Checked
  ( module Data.PrimitiveArray
  , (!)
  ) where

import qualified Data.Vector.Generic as VG

import           Data.PrimitiveArray hiding ((!))

-- | Bounds-checked version of indexing.
--
-- First, we check via @inBounds@, second we check if the linear index is
-- outside of the allocated area.

--(!) :: PrimArrayOps arr sh elm => arr sh elm -> sh -> elm
(!) arr@(Unboxed l h v) idx
  | not (uncurry inBounds (bounds arr) idx) = error "(!) / inBounds: out of bounds!"
  | li < 0 || li >= len = error "(!) / linearIndex: out of bounds!"
  | otherwise = unsafeIndex arr idx
  where li  = linearIndex l h idx
        len = VG.length v
{-# Inline (!) #-}

