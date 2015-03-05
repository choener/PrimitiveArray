
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Subwords span upper triangular tables. A subword (i,j) is legal iff i<=j.
--
-- NOTE Using more complicated shapes has a number of benefits. We don't need
-- to specify triangular or rectangular tables anymore. A rectangular
-- one-dimensional table with a subword as shape actually /does/ create space
-- as required for subwords.
--
-- TODO subword indexing is currently hardcoded to be zero-based. See
-- 'subwordIndex'.
--
-- TODO consider replacing (`quot` 2) with (`shiftR` 1)
--
-- TODO all the QuickCheck stuff is missing

module Data.Array.Repa.Index.Subword where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Aeson
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (quotInt, remInt)
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as M
import qualified Data.Vector.Generic.Base
import qualified Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck
import           Test.QuickCheck.All

import           Data.Array.Repa.Bytes
import           Data.Array.Repa.ExtShape
import           Data.Array.Repa.Index.Outside



stage = "Data.Array.Repa.Index.Subword"

-- | A subword wraps a simple pair.
--
-- Subwords always yield the upper-triangular part of a rect-angular array.
-- This gives the quite curious effect that (0,N) points to the ``largest''
-- index, while (0,0) and (N,N) both point to the smallest. We do, however, use
-- (0,0) as the smallest as (0,k) gives successively smaller upper triangular
-- parts.
--
-- TODO Experiment with @ExtShape Subword@ index generation following
-- larger to smaller diagonals.

newtype Subword = Subword (Int:.Int)
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "Subword"
  [t| Subword -> (Int,Int) |]
  [| \ (Subword (i:.j)) -> (i,j) |]
  [| \ (i,j) -> Subword (i:.j) |]

instance Binary    Subword
instance Serialize Subword
instance FromJSON  Subword
instance ToJSON    Subword

subword :: Int -> Int -> Subword
subword i j = Subword (i:.j)
{-# INLINE subword #-}

-- | triangular numbers
--
-- A000217

triangularNumber :: Int -> Int
triangularNumber x = (x * (x+1)) `quot` 2
{-# INLINE triangularNumber #-}

-- | Size of an upper triangle starting at 'i' and ending at 'j'. "(0,N)" what
-- be the normal thing to use.

upperTri :: Subword -> Int
upperTri (Subword (i:.j)) = triangularNumber $ j-i
{-# INLINE upperTri #-}

-- | Subword indexing. Given the longest subword and the current subword,
-- calculate a linear index "[0,..]". "(l,n)" in this case means "l"ower bound,
-- length "n". And "(i,j)" is the normal index.
--
-- TODO probably doesn't work right with non-zero base ?!

subwordIndex :: Subword -> Subword -> Int
subwordIndex (Subword (l:.n)) (Subword (i:.j)) = adr n (i,j) -- - adr n (l,n)
  where
    adr n (i,j) = n*i - triangularNumber i + j
{-# INLINE subwordIndex #-}

subwordFromIndex :: Subword -> Int -> Subword
subwordFromIndex = error "subwordFromIndex not implemented"
{-# INLINE subwordFromIndex #-}



-- * 'Shape' instances

-- ** The common 'Shape' instance for @(:.Subword)@

-- | Some weird things are going on here. Adding subwords (i,j) and (k,l)
-- yields (i+k,j+l). Normally i==k==0 when calculating space requirements. If
-- you have a subword (3,10) and want the next outer one add (-1,1) and you get
-- what you want. We make NO(!) check that the final subword contains only
-- non-negative indices.

instance Shape sh => Shape (sh :. Subword) where
  {-# INLINE [1] rank #-}
  rank   (sh  :. _)
    = rank sh + 1

  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. Subword (0:.0)

  {-# INLINE [1] unitDim #-}
  unitDim = unitDim :. Subword (0:.1)

  {-# INLINE [1] intersectDim #-}
  intersectDim (sh1 :. Subword (i:.j)) (sh2 :. Subword (k:.l))
    = (intersectDim sh1 sh2 :. Subword (max i k :. min j l))

  {-# INLINE [1] addDim #-}
  addDim (sh1 :. Subword (i:.j)) (sh2 :. Subword (k:.l))
    = addDim sh1 sh2 :. Subword (i+k:.j+l)

  {-# INLINE [1] size #-}
  size  (sh1 :. sw) = size sh1 * upperTri sw

  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. Subword (i:.j))
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
  inShapeRange (zs :. Subword (_:._)) (sh1 :. Subword (l:.n)) (sh2 :. Subword (i:.j))
    = i<=j && l<=i && j<n && (inShapeRange zs sh1 sh2)

  {-# NOINLINE listOfShape #-}
  listOfShape (sh :. Subword (i:.j)) = i : j : listOfShape sh

  {-# NOINLINE shapeOfList #-}
  shapeOfList xx
   = case xx of
    []     -> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
    [x]    -> error $ stage ++ ".toList: only single element remaining!"
    i:j:xs -> shapeOfList xs :. Subword (i:.j)

  {-# INLINE deepSeq #-}
  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

-- |

instance ExtShape sh => ExtShape (sh:.Subword) where
  {-# INLINE subDim #-}
  subDim (sh1:.Subword (i:.j)) (sh2:.Subword (k:.l)) = subDim sh1 sh2 :. Subword (i-k:.j-l)
  {-# INLINE rangeList #-}
  rangeList x y = S.toList $ rangeStream x y
  {-# INLINE rangeStream #-}
  rangeStream (fs:.Subword (0:.0)) (ts:.Subword (0:.t)) = M.flatten mk step Unknown $ rangeStream fs ts where
    mk is = return (is:.t:.t)
    step (is:.k:.l)
      | k<0       = return $ M.Done
      | l>t       = return $ M.Skip                    (is:.k-1:.k-1)
      | otherwise = return $ M.Yield (is:.subword k l) (is:.k  :.l+1)
    {-# INLINE [1] mk #-}
    {-# INLINE [1] step #-}
  {-# INLINE topmostIndex #-}
  topmostIndex (sh1:.Subword (0:.0)) (sh2:.Subword (0:.n)) = topmostIndex sh1 sh2 :. subword 0 n



-- ** Use 'Subword' directly without the need for @(:.)@.

instance Shape Subword where
    {-# INLINE [1] rank #-}
    rank _ = 1
    {-# INLINE [1] zeroDim #-}
    zeroDim = subword 0 0
    {-# INLINE [1] unitDim #-}
    unitDim = subword 0 1
    {-# INLINE [1] intersectDim #-}
    intersectDim (Subword (i:.j)) (Subword (k:.l)) = subword (max i k) (min j l)
    {-# INLINE [1] addDim #-}
    addDim (Subword (i:.j)) (Subword (k:.l)) = subword (i+k) (j+l)
    {-# INLINE [1] size #-}
    size = upperTri
    {-# INLINE [1] sizeIsValid #-}
    sizeIsValid (Subword (i:.j)) = 0<=i && i<=j
    {-# INLINE [1] toIndex #-}
    toIndex = subwordIndex
    {-# INLINE [1] fromIndex #-}
    fromIndex = undefined
    {-# INLINE [1] inShapeRange #-}
    inShapeRange _ (Subword (l:.n)) (Subword (i:.j)) = l<=i && i<=j && j<n
    {-# NOINLINE listOfShape #-}
    listOfShape (Subword (i:.j)) = [i,j]
    {-# NOINLINE shapeOfList #-}
    shapeOfList xs = case xs of [i,j] -> subword i j
                                es    -> error $ stage ++ " toList: " ++ show es
    {-# INLINE deepSeq #-}
    deepSeq sw n = sw `seq` n

instance ExtShape Subword where
    {-# INLINE [1] subDim #-}
    subDim (Subword (i:.j)) (Subword (k:.l)) = subword (i-k) (j-l)
    {-# INLINE rangeList #-}
    rangeList x y = S.toList $ rangeStream x y
    {-# INLINE rangeStream #-}
    rangeStream (Subword (0:.0)) (Subword (0:.t)) = M.flatten mk step Unknown $ M.enumFromStepN t (-1) (t+1) where
      mk k = return (k:.k)
      step (k:.l)
        | l>t       = return $ M.Done
        | otherwise = return $ M.Yield (subword k l) (k:.l+1)
      {-# INLINE [1] mk #-}
      {-# INLINE [1] step #-}
    {-# INLINE topmostIndex #-}
    topmostIndex (Subword (0:.0)) (Subword (0:.n)) = subword 0 n

-- | 'rangeStream' for Outside / Subword is wrong?

instance ExtShape (Outside Subword) where
  {-# INLINE [1] subDim #-}
  subDim (O s) (O t) = O $ subDim s t
  {-# INLINE rangeList #-}
  rangeList x y = S.toList $ rangeStream x y
  {-# INLINE rangeStream #-}
  rangeStream (O (Subword (0:.0))) (O (Subword (0:.t))) = M.flatten mk step Unknown $ M.enumFromStepN 0 1 (t+1) where
    -- TODO step currently does *not* use the method devised with Andrew.
    -- Need to update to ZNE stopping conditition.
    mk k = return (k:.t)
    step (k:.l)
      | l>=k      = return $ M.Yield (O $ subword k l) (k:.l-1)
      | otherwise = return $ M.Done
    {-# INLINE [1] mk   #-}
    {-# INLINE [1] step #-}
  topmostIndex = error "???"
  {-# INLINE topmostIndex #-}



-- * NFData, Arbitrary

-- |

instance NFData Subword where
  rnf (Subword (i:.j)) = i `seq` rnf j

-- |

instance Arbitrary Subword where
  arbitrary = do
    a <- choose (0,100)
    b <- choose (0,100)
    return $ Subword (min a b :. max a b)
  shrink (Subword (i:.j))
    | i<j       = [Subword (i:.j-1)]
    | otherwise = []

instance Arbitrary (Outside Subword) where
  arbitrary = O <$> arbitrary
  shrink (O z) = map O $ shrink z

instance Arbitrary z => Arbitrary (z:.Subword) where
  arbitrary = (:.) <$> arbitrary <*> arbitrary
  shrink (z:.s) = (:.) <$> shrink z <*> shrink s

instance Arbitrary z => Arbitrary (z:.Outside Subword) where
  arbitrary = (:.) <$> arbitrary <*> arbitrary
  shrink (z:.s) = (:.) <$> shrink z <*> shrink s

{-
instance IndexLens Subword where
  _from f (Subword (i:.j)) = fmap (\i' -> subword i' j ) (f i)
  _to   f (Subword (i:.j)) = fmap (\j' -> subword i  j') (f j)
  {-# INLINE _from #-}
  {-# INLINE _to   #-}
-}

