{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Outside' covers subwords for outside calculations. These types of
-- calculations requires quite "weird" index movements if you want to stay with
-- usual grammars. This remains true if grammars are transformed to Chomsky
-- normal form, only that in said form it is easier to write down the
-- recursions.
--
-- TODO basically untested!

module Data.Array.Repa.Index.Outside where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (quotInt, remInt)
import qualified Data.Vector.Generic.Base
import qualified Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck
import           Test.QuickCheck.All

import           Data.Array.Repa.ExtShape
import           Data.Array.Repa.Index.Subword hiding (upperTri, subwordIndex, subwordFromIndex)
import qualified Data.Array.Repa.Index.Subword as SW



stage = "Data.Array.Repa.Index.Outside"

-- | 'Outside' inverts the usual subword (i,j) system.
--
-- TODO do I need to store N ?

newtype Outside = Outside (Int:.Int)
  deriving (Eq,Ord,Show)

derivingUnbox "Outside"
  [t| Outside -> (Int,Int) |]
  [| \ (Outside (i:.j)) -> (i,j) |]
  [| \ (i,j) -> Outside (i:.j) |]

outside :: Int -> Int -> Outside
outside i j = Outside (i:.j)
{-# INLINE outside #-}

-- | Size of an upper triangle starting at 'i' and ending at 'j'. "(0,N)" what
-- be the normal thing to use. Internally, we stell upper triangular matrices.

upperTri :: Outside -> Int
upperTri (Outside (i:.j)) = triangularNumber $ j-i
{-# INLINE upperTri #-}

-- | Outside indexing. Given the longest subword and the current subword,
-- calculate a linear index "[0,..]". "(l,n)" in this case means "l"ower bound,
-- length "n". And "(i,j)" is the normal index.
--
-- TODO probably doesn't work right with non-zero base ?!

subwordIndex :: Outside -> Outside -> Int
subwordIndex (Outside (l:.n)) (Outside (i:.j)) = adr n (i,j) -- - adr n (l,n)
  where
    adr n (i,j) = n*i - triangularNumber i + j
{-# INLINE subwordIndex #-}

subwordFromIndex :: Outside -> Int -> Outside
subwordFromIndex = error "not implemented"
{-# INLINE subwordFromIndex #-}



-- | Some weird things are going on here. Adding subwords (i,j) and (k,l)
-- yields (i+k,j+l). Normally i==k==0 when calculating space requirements. If
-- you have a subword (3,10) and want the next outer one add (-1,1) and you get
-- what you want. We make NO(!) check that the final subword contains only
-- non-negative indices.

instance Shape sh => Shape (sh :. Outside) where
  {-# INLINE [1] rank #-}
  rank   (sh  :. _)
    = rank sh + 1
  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. Outside (0:.0)

  {-# INLINE [1] unitDim #-}
  unitDim = unitDim :. Outside (0:.1)

  {-# INLINE [1] intersectDim #-}
  intersectDim (sh1 :. Outside (i:.j)) (sh2 :. Outside (k:.l))
    = (intersectDim sh1 sh2 :. Outside (max i k :. min j l))

  {-# INLINE [1] addDim #-}
  addDim (sh1 :. Outside (i:.j)) (sh2 :. Outside (k:.l))
    = addDim sh1 sh2 :. Outside (i+k:.j+l)

  {-# INLINE [1] size #-}
  size  (sh1 :. sw) = size sh1 * upperTri sw

  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. Outside (i:.j))
    | size sh1 > 0
    = i>=0 && i<=j && j <= maxBound `div` size sh1
    | otherwise
    = False

  {-# INLINE [1] toIndex #-}
  toIndex (sh1 :. sh2) (sh1' :. sh2')
    = toIndex sh1 sh1' * upperTri sh2 + subwordIndex sh2 sh2'

  {-# INLINE [1] fromIndex #-}
  fromIndex (ds :. d) n  = undefined -- fromIndex ds (n `quotInt` d) :. r
    where
      r = subwordFromIndex d n
    -- If we assume that the index is in range, there is no point
    -- in computing the remainder for the highest dimension since
    -- n < d must hold. This saves one remInt per element access which
    -- is quite a big deal.
    {-
    r       | rank ds == 0  = n
            | otherwise     = n `remInt` d -}

  -- | TODO fix for lower bounds check!
  {-# INLINE [1] inShapeRange #-}
  inShapeRange (zs :. Outside (_:._)) (sh1 :. Outside (l:.n)) (sh2 :. Outside (i:.j))
    = i<=j && l<=i && j<n && (inShapeRange zs sh1 sh2)

  {-# NOINLINE listOfShape #-}
  listOfShape (sh :. Outside (i:.j)) = i : j : listOfShape sh

  {-# NOINLINE shapeOfList #-}
  shapeOfList xx
   = case xx of
    []     -> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
    [x]    -> error $ stage ++ ".toList: only single element remaining!"
    i:j:xs -> shapeOfList xs :. Outside (i:.j)

  {-# INLINE deepSeq #-}
  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

-- |

instance ExtShape sh => ExtShape (sh:.Outside) where
  subDim (sh1:.Outside (i:.j)) (sh2:.Outside (k:.l)) = subDim sh1 sh2 :. Outside (i-k:.j-l)
  {-# INLINE subDim #-}
  rangeList (sh1:.Outside (i:.j)) (sh2:.Outside (k:.l)) = error "not implemented" -- [sh:.Outside (m,n) | sh <- rangeList sh1 sh2, m <- [i .. [i+k], n <- [ n <- [n1 .. (n1+n2) ] ]
  {-# INLINE rangeList #-}

-- |

instance NFData Outside where
  rnf (Outside (i:.j)) = i `seq` rnf j

-- |

instance Arbitrary Outside where
  arbitrary = do
    a <- choose (0,100)
    b <- choose (0,100)
    return $ Outside (min a b :. max a b)
  shrink (Outside (i:.j))
    | i<j       = [Outside (i:.j-1)]
    | otherwise = []

instance Arbitrary z => Arbitrary (z:.Outside) where
  arbitrary = (:.) <$> arbitrary <*> arbitrary
  shrink (z:.s) = (:.) <$> shrink z <*> shrink s

