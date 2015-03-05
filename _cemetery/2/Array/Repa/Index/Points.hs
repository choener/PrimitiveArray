
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Index structures for left- and right-linear grammars. Do not use this
-- index for general linear- or context-free grammars.
--
-- Internally, both 'PointL' and 'PointR' work a lot like 'Subword's, but in
-- non-terminals we only store the left- or right part.

module Data.Array.Repa.Index.Points where

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
import qualified Data.Vector.Fusion.Stream.Monadic as M
import qualified Data.Vector.Generic.Base
import qualified Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck
import           Test.QuickCheck.All

import           Data.Array.Repa.Bytes
import           Data.Array.Repa.ExtShape
import           Data.Array.Repa.Index.Outside



stage = "Data.Array.Repa.Index.Points"

-- | A point in left-linear grammars. In @(i:.j)@, @j@ is the non-terminal
-- storage point, @i==0@ always for the non-terminal, while @i>=0@ for
-- terminals, which are on the right of the non-terminal. (This is why
-- left-linear grammars are called left-linear: they recurse on the left).
--
-- PS: all this left/right talk deals with the RHS of a production rule, the
-- LHS is always a non-terminal ;-)

newtype PointL = PointL (Int:.Int)
  deriving (Eq,Read,Show,Generic)

pointL :: Int -> Int -> PointL
pointL i j = PointL (i:.j)
{-# INLINE pointL #-}

-- | A point in right-linear grammars.

newtype PointR = PointR (Int:.Int)
  deriving (Eq,Read,Show,Generic)

pointR :: Int -> Int -> PointR
pointR i j = PointR (i:.j)
{-# INLINE pointR #-}



-- * Instances: PointL

derivingUnbox "PointL"
  [t| PointL -> (Int,Int) |]
  [| \ (PointL (i:.j)) -> (i,j) |]
  [| \ (i,j) -> PointL (i:.j) |]

instance Binary    PointL
instance Serialize PointL
instance FromJSON  PointL
instance ToJSON    PointL

instance Shape PointL where
  {-# INLINE [1] rank #-}
  rank _ = 1

  {-# INLINE [1] zeroDim #-}
  zeroDim = PointL (0:.0)

  {-# INLINE [1] unitDim #-}
  unitDim = PointL (0:.1)

  {-# INLINE [1] intersectDim #-}
  intersectDim (PointL (i:.j)) (PointL (k:.l))
    = PointL (max i k :. min j l)

  {-# INLINE [1] addDim #-}
  addDim (PointL (i:.j)) (PointL (k:.l))
    = PointL (i+k:.j+l)

  {-# INLINE [1] size #-}
  size  (PointL (i:.j)) = (j-i)

  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (PointL (i:.j))
    = i>=0 && i<=j && j <= maxBound

  {-# INLINE [1] toIndex #-}
  toIndex (PointL(l:.r)) (PointL(i:.j))
    = (j-l)

  {-# INLINE [1] fromIndex #-}
  fromIndex d n  = undefined

  {-# INLINE [1] inShapeRange #-}
  inShapeRange (PointL (_:._)) (PointL (l:.n)) (PointL (i:.j))
    = i<=j && l<=i && j<n

  {-# NOINLINE listOfShape #-}
  listOfShape (PointL (i:.j)) = i : j : []

  {-# NOINLINE shapeOfList #-}
  shapeOfList xx
   = case xx of
    []     -> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
    [x]    -> error $ stage ++ ".toList: only single element remaining!"
    i:j:[] -> PointL (i:.j)

  {-# INLINE deepSeq #-}
  deepSeq n x = n `seq` x

instance ExtShape PointL where
  {-# INLINE [1] subDim #-}
  subDim (PointL (i:.j)) (PointL (k:.l)) = PointL (i-k:.j-l)
  {-# INLINE [1] rangeList #-}
  rangeList _ _ = error "PointL:rangeList not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (PointL (0:.f)) (PointL (0:.t)) = M.map (pointL 0) $ M.enumFromStepN f 1 (t-f+1)
  {-# INLINE topmostIndex #-}
  topmostIndex (PointL (0:.f)) (PointL (0:.t)) = pointL 0 t

instance ExtShape (Outside PointL) where
  {-# INLINE [1] subDim #-}
  subDim (O ij) (O kl) = O $ subDim ij kl
  {-# INLINE [1] rangeList #-}
  rangeList _ _ = error "PointL:rangeList not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (O (PointL (0:.f))) (O (PointL (0:.t)))
    = M.map (O . pointL 0) $ M.enumFromStepN t (-1) (t-f+1)
  {-# INLINE topmostIndex #-}
  topmostIndex (O (PointL (0:.f))) (O (PointL (0:.t))) = O $ pointL 0 f



instance Shape sh => Shape (sh :. PointL) where
  {-# INLINE [1] rank #-}
  rank   (sh  :. _)
    = rank sh + 1

  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. PointL (0:.0)

  {-# INLINE [1] unitDim #-}
  unitDim = unitDim :. PointL (0:.1)

  {-# INLINE [1] intersectDim #-}
  intersectDim (sh1 :. PointL (i:.j)) (sh2 :. PointL (k:.l))
    = (intersectDim sh1 sh2 :. PointL (max i k :. min j l))

  {-# INLINE [1] addDim #-}
  addDim (sh1 :. PointL (i:.j)) (sh2 :. PointL (k:.l))
    = addDim sh1 sh2 :. PointL (i+k:.j+l)

  -- NOTE size is calculated NOT as upper-triangular, but linear!
  {-# INLINE [1] size #-}
  size  (sh1 :. PointL (i:.j)) = size sh1 * (j-i)

  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. PointL (i:.j))
    | size sh1 > 0
    = i>=0 && i<=j && j <= maxBound `div` size sh1
    | otherwise
    = False

  -- NOTE only the @j@ coordinate is used for indexing NTs, @i@ is just for
  -- convenience. @l@ however restricts the NT to some value @>0@ if desired.
  {-# INLINE [1] toIndex #-}
  toIndex (sh1 :. PointL(l:.r)) (sh1' :. PointL(i:.j))
    = toIndex sh1 sh1' * (r-l) + (j-l)

  {-# INLINE [1] fromIndex #-}
  fromIndex (ds :. d) n  = undefined -- fromIndex ds (n `quotInt` d) :. r
    where
      r = undefined

  -- | TODO fix for lower bounds check!
  {-# INLINE [1] inShapeRange #-}
  inShapeRange (zs :. PointL (_:._)) (sh1 :. PointL (l:.n)) (sh2 :. PointL (i:.j))
    = i<=j && l<=i && j<n && (inShapeRange zs sh1 sh2)

  {-# NOINLINE listOfShape #-}
  listOfShape (sh :. PointL (i:.j)) = i : j : listOfShape sh

  {-# NOINLINE shapeOfList #-}
  shapeOfList xx
   = case xx of
    []     -> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
    [x]    -> error $ stage ++ ".toList: only single element remaining!"
    i:j:xs -> shapeOfList xs :. PointL (i:.j)

  {-# INLINE deepSeq #-}
  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

instance ExtShape sh => ExtShape (sh:.PointL) where
  {-# INLINE [1] subDim #-}
  subDim (sh1:.PointL (i:.j)) (sh2:.PointL (k:.l)) = subDim sh1 sh2 :. PointL (i-k:.j-l)
  {-# INLINE [1] rangeList #-}
  rangeList _ _ = error "PointL:rangeList not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (fs:.PointL (0:.f)) (ts:.PointL (0:.t)) = M.flatten mk step Unknown $ rangeStream fs ts where
    mk is = return (is:.f)
    step (is:.k)
      | k>t       = return $ M.Done
      | otherwise = return $ M.Yield (is:.pointL 0 k) (is:.(k+1))
    {-# INLINE [1] mk #-}
    {-# INLINE [1] step #-}
  topmostIndex (sh1:.PointL (0:.f)) (sh2:.PointL (0:.t)) = topmostIndex sh1 sh2 :. pointL 0 t
  {-# INLINE topmostIndex #-}


instance NFData PointL where
  rnf (PointL (i:.j)) = i `seq` rnf j
  {-# INLINE rnf #-}

-- TODO maybe vary the left border, too? Since this invalidates that @i==0@ in
-- @PointL (i:.j)@, we would need to make sure that the memoizers for NTs get
-- notified ...

instance Arbitrary PointL where
  arbitrary = do
    b <- choose (0,100)
    return $ pointL 0 b
  shrink (PointL (i:.j))
    | i<j = [pointL i $ j-1]
    | otherwise = []

instance Arbitrary z => Arbitrary (z:.PointL) where
  arbitrary = (:.) <$> arbitrary <*> arbitrary
  shrink (z:.s) = (:.) <$> shrink z <*> shrink s



-- * Instances: PointR

derivingUnbox "PointR"
  [t| PointR -> (Int,Int) |]
  [| \ (PointR (i:.j)) -> (i,j) |]
  [| \ (i,j) -> PointR (i:.j) |]

instance Binary    PointR
instance Serialize PointR
instance FromJSON  PointR
instance ToJSON    PointR

instance Shape sh => Shape (sh :. PointR) where
  {-# INLINE [1] rank #-}
  rank   (sh  :. _)
    = rank sh + 1

  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. PointR (0:.0)

  {-# INLINE [1] unitDim #-}
  unitDim = unitDim :. PointR (0:.1)

  {-# INLINE [1] intersectDim #-}
  intersectDim (sh1 :. PointR (i:.j)) (sh2 :. PointR (k:.l))
    = (intersectDim sh1 sh2 :. PointR (max i k :. min j l))

  {-# INLINE [1] addDim #-}
  addDim (sh1 :. PointR (i:.j)) (sh2 :. PointR (k:.l))
    = addDim sh1 sh2 :. PointR (i+k:.j+l)

  -- NOTE size is calculated NOT as upper-triangular, but linear!
  {-# INLINE [1] size #-}
  size  (sh1 :. PointR (i:.j)) = size sh1 * (j-i)

  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. PointR (i:.j))
    | size sh1 > 0
    = i>=0 && i<=j && j <= maxBound `div` size sh1
    | otherwise
    = False

  -- NOTE only the @i@ coordinate is used for indexing NTs, @j@ is just for
  -- convenience. @l@ however restricts the NT to some value @>0@ if desired.
  {-# INLINE [1] toIndex #-}
  toIndex (sh1 :. PointR(l:.r)) (sh1' :. PointR(i:.j))
    = toIndex sh1 sh1' * (r-l) + i

  {-# INLINE [1] fromIndex #-}
  fromIndex (ds :. d) n  = undefined -- fromIndex ds (n `quotInt` d) :. r
    where
      r = undefined

  -- | TODO fix for lower bounds check!
  {-# INLINE [1] inShapeRange #-}
  inShapeRange (zs :. PointR (_:._)) (sh1 :. PointR (l:.n)) (sh2 :. PointR (i:.j))
    = i<=j && l<=i && j<n && (inShapeRange zs sh1 sh2)

  {-# NOINLINE listOfShape #-}
  listOfShape (sh :. PointR (i:.j)) = i : j : listOfShape sh

  {-# NOINLINE shapeOfList #-}
  shapeOfList xx
   = case xx of
    []     -> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
    [x]    -> error $ stage ++ ".toList: only single element remaining!"
    i:j:xs -> shapeOfList xs :. PointR (i:.j)

  {-# INLINE deepSeq #-}
  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

instance ExtShape sh => ExtShape (sh:.PointR) where
  {-# INLINE [1] subDim #-}
  subDim (sh1:.PointR (i:.j)) (sh2:.PointR (k:.l)) = subDim sh1 sh2 :. PointR (i-k:.j-l)
  {-# INLINE [1] rangeList #-}
  rangeList _ _ = error "PointR:rangeList not implemented"
  {-# INLINE rangeStream #-}
  rangeStream _ _ = error "PointR:rangeStream not implemented"
  topmostIndex _ _ = error "PointR:topmostIndex not implemented"

instance NFData PointR where
  rnf (PointR (i:.j)) = i `seq` rnf j
  {-# INLINE rnf #-}

instance Arbitrary PointR where
  arbitrary = do
    b <- choose (0,100)
    return $ pointR b 100
  shrink (PointR (i:.j))
    | i<j = [pointR (i+1) $ j]
    | otherwise = []

instance Arbitrary z => Arbitrary (z:.PointR) where
  arbitrary = (:.) <$> arbitrary <*> arbitrary
  shrink (z:.s) = (:.) <$> shrink z <*> shrink s

