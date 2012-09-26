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

