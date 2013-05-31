{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | Additional functions on shapes

module Data.Array.Repa.ExtShape where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape



-- | A number of additional operations that are useful together with
-- 'PrimitiveArray's.

class ExtShape sh where

  -- | subtract the right coordinates from the left. Does not check if the
  -- resulting shape make sense.

  subDim :: sh -> sh -> sh

  -- | Given an index and an extend, return a list of all indices. For
  -- @rangeList (Z:.3) (Z:.2)@ this returns @[(Z:.3), (Z:.4), (Z:.5)]@.

  rangeList :: sh -> sh -> [sh]



instance ExtShape Z where
  subDim _ _ = Z
  {-# INLINE subDim #-}
  rangeList _ _ = [Z]
  {-# INLINE rangeList #-}

instance ExtShape sh => ExtShape (sh:.Int) where
  subDim (sh1:.n1) (sh2:.n2) = subDim sh1 sh2 :. (n1-n2)
  {-# INLINE subDim #-}
  rangeList (sh1:.n1) (sh2:.n2) = [sh:.n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2) ] ]
  {-# INLINE rangeList #-}
