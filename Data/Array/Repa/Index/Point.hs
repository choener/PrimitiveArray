{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | A Repa-compatible index of points in multi-dimensional space. A point
-- represents the index of a left- or right-linear grammar.

module Data.Array.Repa.Index.Point where

import Control.Applicative
import Control.DeepSeq
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import GHC.Base (quotInt, remInt)
import Test.QuickCheck
import Test.QuickCheck.All

import Data.Array.Repa.ExtShape



stage = "Data.Array.Repa.Index.Point"

-- |

newtype Point = Point Int
  deriving (Eq,Ord,Show)

-- |

instance Shape sh => Shape (sh :. Point) where
  {-# INLINE [1] rank #-}
  rank   (sh  :. _)
    = rank sh + 1

  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. Point 0

  {-# INLINE [1] unitDim #-}
  unitDim = unitDim :. Point 1

  {-# INLINE [1] intersectDim #-}
  intersectDim (sh1 :. Point n1) (sh2 :. Point n2)
    = (intersectDim sh1 sh2 :. Point (min n1 n2))

  {-# INLINE [1] addDim #-}
  addDim (sh1 :. Point n1) (sh2 :. Point n2)
    = addDim sh1 sh2 :. Point (n1 + n2)

  {-# INLINE [1] size #-}
  size  (sh1 :. Point n)
    = size sh1 * n

  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. Point n)
    | size sh1 > 0
    = n <= maxBound `div` size sh1

    | otherwise
    = False

  {-# INLINE [1] toIndex #-}
  toIndex (sh1 :. Point sh2) (sh1' :. Point sh2')
    = toIndex sh1 sh1' * sh2 + sh2'

  {-# INLINE [1] fromIndex #-}
  fromIndex (ds :. Point d) n
          = fromIndex ds (n `quotInt` d) :. Point r
          where
          -- If we assume that the index is in range, there is no point
          -- in computing the remainder for the highest dimension since
          -- n < d must hold. This saves one remInt per element access which
          -- is quite a big deal.
          r       | rank ds == 0  = n
                  | otherwise     = n `remInt` d

  {-# INLINE [1] inShapeRange #-}
  inShapeRange (zs :. Point z) (sh1 :. Point n1) (sh2 :. Point n2)
    = (n2 >= z) && (n2 < n1) && (inShapeRange zs sh1 sh2)

  {-# NOINLINE listOfShape #-}
  listOfShape (sh :. Point n)
   = n : listOfShape sh

  {-# NOINLINE shapeOfList #-}
  shapeOfList xx
   = case xx of
    []  -> error $ stage ++ ".toList: empty list when converting to  (_ :. Point)"
    x:xs  -> shapeOfList xs :. Point x

  {-# INLINE deepSeq #-}
  deepSeq (sh :. Point n) x = deepSeq sh (n `seq` x)

-- |

instance ExtShape sh => ExtShape (sh:.Point) where
  subDim (sh1:.Point n1) (sh2:.Point n2) = subDim sh1 sh2 :. Point (n1-n2)
  {-# INLINE subDim #-}
  rangeList (sh1:.Point n1) (sh2:.Point n2) = [sh:.Point n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2) ] ]
  {-# INLINE rangeList #-}

instance NFData Point where
  rnf (Point i) = rnf i

-- |

instance Arbitrary Point where
  arbitrary = Point <$> choose (0,100)
  shrink (Point i)
    | i>0       = [Point (i-1)]
    | otherwise = []

instance Arbitrary z => Arbitrary (z:.Point) where
  arbitrary = (:.) <$> arbitrary <*> arbitrary
  shrink (z:.s) = (:.) <$> shrink z <*> shrink s

