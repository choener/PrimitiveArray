{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Unboxed triangular arrays.

module Data.PrimitiveArray.UpperTriangular.Unboxed where

import Control.Monad
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Control.Exception (assert)
import Data.Vector.Unboxed as VU hiding (forM_, length, zipWithM_)
import Data.Vector.Unboxed.Mutable as VUM hiding (length)

import Data.ExtShape
import Data.PrimitiveArray
import Data.PrimitiveArray.FillTable



-- | Monadic arrays of primitive type.

data MUpTri s sh elm = MUpTri !sh !(MVector s elm)

-- | Immutable arrays of primitive type.

data UpTri sh elm = UpTri !sh !(Vector elm)

-- | Associate data types via type family

type instance MutArray UpTri = MUpTri

-- | How to fill an upper triangular table. The diagonals will form hyperplanes
-- in hypercubes. This function needs to be polymorphic on the shape 'sh', as
-- we want to support more than just two dimensions.
--
-- TODO: Parallel filling of such tables is not trivial. Maybe we should pull
-- in Repa and use their gangs.

instance (Monad m) => FillDiagonal m (MUpTri s sh elm, sh -> elm) where
  fillDiagonalS (tbl,f) = return ()
  -- | TODO There are two possibilities: create a temporary array, fill it in
  -- parallel using 'parMap' or whatever. Then write to the final array. Or use
  -- the Repa ideas of a gang and go from there, with some "STtoIO" in there.
  -- Needs thought.
  fillDiagonalP (tbl,f) = error "fillDiagonalP / Triangular.Unboxed undefined"
