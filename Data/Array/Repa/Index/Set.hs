
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Index structures for unordered data types. We use this for Hamiltonian
-- path problems, where we need sets with an interface.

module Data.Array.Repa.Index.Set where

import Data.Aeson
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Binary
import Data.Bits
import Data.Serialize
import Data.Vector.Unboxed.Deriving
import GHC.Generics

import Data.Bits.Ordered
import Data.Array.Repa.ExtShape



-- | a path set denotes a set of visited nodes 'psSet' together with the
-- node that was visited first 'psFirst' and the node visited last
-- 'psLast'.
--
-- NOTE we use 'Int' here, as @Int@s have good optimization and many
-- operations assume @Int@s instead of @Word@s.
--
-- TODO should @psSet@ contain @psFirst@ and @psLast@ or not? Currently:
-- yes! (we want to differentiate having visited nothing (all @PathSet
-- 0 0 0@) to having visited just node 0 @(PathSet 1 0 0)@. Have @psFirst@
-- and @psLast@ in @psSet@ increases the memory requirements to @N*N*2^N@
-- from @N*N*2^(N-2)@ i.e. by a factor of 4.
--
-- TODO newtype PathSet = PathSet { Z:.Int:.Int:.Int }

data PathSet = PathSet
  { psSet   :: !Int
  , psFirst :: !Int
  , psLast  :: !Int
  }
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "PathSet"
  [t| PathSet -> (Int,Int,Int) |]
  [| \ (PathSet s f l) -> (s,f,l) |]
  [| \ (s,f,l) -> PathSet s f l |]

instance Binary    PathSet
instance Serialize PathSet
instance ToJSON    PathSet
instance FromJSON  PathSet



instance Shape sh => Shape (sh:.PathSet) where
  {-# INLINE [1] rank #-}
  rank (sh:._) = rank sh + 1
  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. PathSet 0 0 0
  {-# INLINE [1] unitDim #-}
  unitDim = unitDim :. PathSet 1 0 0
  {-# INLINE [1] intersectDim #-}
  intersectDim = error "sh:.PathSet / intersectDim"
  {-# INLINE [1] addDim #-}
  addDim = error "do not use addDim for the PrimitiveArray stuff"
  {-# INLINE [1] size #-}
  size (sh1 :. PathSet s _ _)
    = let !p = popCount s in size (sh1:.s+1:.p:.p)
    -- let p = popCount s + 1 in size sh1 * s * p * p
  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. PathSet p _ _)
    | size sh1 > 0 = p < maxBound `div` size sh1
    | otherwise    = False
  {-# INLINE [1] toIndex #-}
  -- Recart the calculation in terms known to repa
  -- TODO check this!
  toIndex (shF :. PathSet sS fF lL) (sh :. PathSet s f l)
    = let !p = popCount sS
      in  toIndex (shF:.sS:.p:.p) (sh:.s:.f:.l)
  {-
    = let p = popCount sS + 1
      in  toIndex shF sh * (sS * p * p)
          + s * p * p + f * p + l
  -}
  {-# INLINE [1] fromIndex #-}
  fromIndex = error "sh:.PathSet / fromIndex"
  {-# INLINE [1] inShapeRange #-}
  inShapeRange = error "sh:.PathSet / inShapeRange"
  {-# NOINLINE listOfShape #-}
  listOfShape = error "sh:.PathSet / listOfShape"
  {-# NOINLINE shapeOfList #-}
  shapeOfList = error "sh:.PathSet / shapeOfList"
  {-# INLINE deepSeq #-}
  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

{-
instance ExtShape sh => ExtShape (sh:.PathSet) where
  {-# INLINE [1] subDim #-}
  -- TODO check this!
  subDim (sh1:.PathSet s _ _) (sh2:.PathSet t _ _)
    = let p = popCount (s-t)
      in  subDim sh1 sh2 :. PathSet (s-t) p p
-}

