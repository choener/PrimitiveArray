
module Data.PrimitiveArray 
  ( module Data.Array.Repa.Index
  , module Data.Array.Repa.Bytes
  , module Data.Array.Repa.ExtShape
  , module Data.Array.Repa.Index.Lens
  , module Data.Array.Repa.Index.Outside
  , module Data.Array.Repa.Index.Point
  , module Data.Array.Repa.Index.Points
  , module Data.Array.Repa.Index.Subword
  , module Data.PrimitiveArray.Class
  , module Data.PrimitiveArray.FillTables
  , module Data.PrimitiveArray.QuickCheck
  , module Data.PrimitiveArray.Zero
  ) where

import Data.Array.Repa.Index ( (:.) (..), Z (..) )

import Data.Array.Repa.Bytes
import Data.Array.Repa.ExtShape
import Data.Array.Repa.Index.Lens
import Data.Array.Repa.Index.Outside hiding (stage,upperTri,subwordIndex,subwordFromIndex)
import Data.Array.Repa.Index.Point hiding (stage)
import Data.Array.Repa.Index.Points hiding (stage)
import Data.Array.Repa.Index.Subword hiding (stage,upperTri,subwordIndex,subwordFromIndex)
import Data.PrimitiveArray.Class
import Data.PrimitiveArray.FillTables
import Data.PrimitiveArray.QuickCheck
import Data.PrimitiveArray.Zero

