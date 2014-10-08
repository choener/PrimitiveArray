
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | A general wrapper to transform @Inside@-style indices into
-- @Outside@-style indices.
--
-- We can't automate this completely, as we want to fill tables in
-- a different order, for example. Therefor, each index module needs to
-- import the @Outside@ module, then derive the required @Shape@ instances.

module Data.Array.Repa.Index.Outside where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics



-- | An outside index is specified by the max bounds 'oMax' and the current
-- index 'oO'.
--
-- 'Shape' operations have a single generic instance. 'ExtShape' instances
-- need to be specialized for each concrete instance.

newtype Outside z = O { unO :: z }
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "Outside"
  [t| forall z . Unbox z => Outside z -> z |]
  [| unO |]
  [| O   |]

instance Binary    z => Binary    (Outside z)
instance Serialize z => Serialize (Outside z)
instance ToJSON    z => ToJSON    (Outside z)
instance FromJSON  z => FromJSON  (Outside z)

instance (Eq z, Shape sh, Shape (sh:.z)) => Shape (sh:.Outside z) where
  {-# INLINE [1] rank #-}
  rank (sh:._) = rank sh + 1
  {-# INLINE [1] zeroDim #-}
  zeroDim = let (sh:.z) = zeroDim in sh :. O z
  {-# INLINE [1] unitDim #-}
  unitDim = let (sh:.z) = unitDim in sh :. O z
  {-# INLINE [1] intersectDim #-}
  intersectDim = error "sh:.Outside / intersectDim"
  {-# INLINE [1] addDim #-}
  addDim (sh1 :. O z1) (sh2 :. O z2)
    = let (sh:.z) = addDim (sh1:.z1) (sh2:.z2) in sh :. O z
  {-# INLINE [1] size #-}
  size (sh1 :. O z) = size (sh1 :. z)
  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. O z) = sizeIsValid (sh1:.z)
  {-# INLINE [1] toIndex #-}
  toIndex (shF :. O zF) (sh :. O z)
    = toIndex (shF :. zF) (sh :. z)
  {-# INLINE [1] fromIndex #-}
  fromIndex = error "sh:.PathSet / fromIndex"
  {-# INLINE [1] inShapeRange #-}
  inShapeRange = error "sh:.PathSet / inShapeRange"
  {-# NOINLINE listOfShape #-}
  listOfShape = error "sh:.PathSet / listOfShape"
  {-# NOINLINE shapeOfList #-}
  shapeOfList = error "sh:.PathSet / shapeOfList"
  {-# INLINE deepSeq #-}
  deepSeq (sh :. O n) x = deepSeq sh (n `seq` x)

instance NFData z => NFData (Outside z) where
  rnf (O z) = rnf z
  {-# INLINE rnf #-}

