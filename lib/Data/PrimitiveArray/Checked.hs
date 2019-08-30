
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
(!) arr@(Dense h v) idx
  | not (inBounds (upperBound arr) idx) = error $ "(!) / inBounds: out of bounds! " ++ show (h,idx)
  | li < 0 || li >= len = error $ "(!) / linearIndex: out of bounds! " ++ show (h,li,len,idx)
  | otherwise = unsafeIndex arr idx
  where li  = linearIndex h idx
        len = VG.length v
{-# Inline (!) #-}

