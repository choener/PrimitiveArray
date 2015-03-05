{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | Additional functions on shapes. @rangeStream@ is defined here for
-- @(sh:.Int)@ with an arbitrary ordering from smaller to larger values.

module Data.Array.Repa.ExtShape where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Vector.Fusion.Stream.Monadic (Stream, singleton, Step (Done, Yield), flatten)
import Data.Vector.Fusion.Stream.Size (Size (Unknown))



-- | A number of additional operations that are useful together with
-- 'PrimitiveArray's.

class ExtShape sh where

  -- | subtract the right coordinates from the left. Does not check if the
  -- resulting shape make sense.

  subDim :: sh -> sh -> sh

  -- | Given inclusive lower and upper bounds for the shape, return
  -- a stream of all included indices. This stream should produce indices
  -- in a somewhat "/natural/" order. Since this library is intended as
  -- a low-level library for ADPfusion dynamic programming, the existing
  -- instances are in a way that produces an index-stream suitable for
  -- typical DP tasks. This means, for example, that @Subword@s advance
  -- from small to large ones, with the most upper-right point @(0,N)@
  -- being the last one, as it holds the CYK result for the completed
  -- parse.

  rangeStream :: Monad m => sh -> sh -> Stream m sh

  -- | Given an index and an extend, return a list of all indices. For
  -- @rangeList (Z:.3) (Z:.2)@ this returns @[(Z:.3), (Z:.4), (Z:.5)]@.
  --
  -- TODO deprecate this function

  rangeList :: sh -> sh -> [sh]

  -- | Returns the topmost index for CYK-style parsing. Requires the lower
  -- and upper bound as, for example, returned by
  -- @Data.PrimitiveArray.Class.bounds@

  topmostIndex :: sh -> sh -> sh



instance ExtShape Z where
  subDim _ _ = Z
  {-# INLINE subDim #-}
  rangeList _ _ = [Z]
  {-# INLINE rangeList #-}
  rangeStream Z Z = singleton Z
  {-# INLINE rangeStream #-}
  topmostIndex Z Z = Z
  {-# INLINE topmostIndex #-}

instance ExtShape sh => ExtShape (sh:.Int) where
  subDim (sh1:.n1) (sh2:.n2) = subDim sh1 sh2 :. (n1-n2)
  {-# INLINE subDim #-}
  rangeList (sh1:.n1) (sh2:.n2) = [sh:.n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2) ] ]
  {-# INLINE rangeList #-}
  rangeStream (fs:.f) (ts:.t) = flatten mk step Unknown $ rangeStream fs ts where
    mk is = return (is:.f)
    step (is:.k)
      | k>t       = return $ Done
      | otherwise = return $ Yield (is:.k) (is:.k+1)
    {-# INLINE [1] mk #-}
    {-# INLINE [1] step #-}
  {-# INLINE rangeStream #-}
  topmostIndex (sh1:.n1) (sh2:.n2) = topmostIndex sh1 sh2 :. n2
  {-# INLINE topmostIndex #-}

