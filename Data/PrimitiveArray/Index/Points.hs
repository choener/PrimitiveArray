
{-# Language TypeOperators #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}

module Data.PrimitiveArray.Index.Points where

import           Data.Aeson
import           Data.Binary
import           Data.Bits
import           Data.Bits.Extras (Ranked)
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU

import           Data.PrimitiveArray.Index.Class



-- | A point in a right-linear grammar.

newtype PointL = PointL (Int:.Int)
  deriving (Eq,Read,Show,Generic)

pointL :: Int -> Int -> PointL
pointL i j = PointL (i:.j)
{-# INLINE pointL #-}

-- | A point in a right-linear grammars.

newtype PointR = PointR (Int:.Int)
  deriving (Eq,Read,Show,Generic)

pointR :: Int -> Int -> PointR
pointR i j = PointR (i:.j)
{-# INLINE pointR #-}



derivingUnbox "PointL"
  [t| PointL -> (Int,Int) |]
  [| \ (PointL (i:.j)) -> (i,j) |]
  [| \ (i,j) -> PointL (i:.j) |]

instance Binary    PointL
instance Serialize PointL
instance FromJSON  PointL
instance ToJSON    PointL

instance Index PointL where
  linearIndex l _ (PointL (_:.z)) = z - smallestLinearIndex l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (PointL (l:._)) = l -- NOTE only the smallest left part is interesting
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (PointL (_:.h)) = h
  {-# INLINE largestLinearIndex #-}
  size (PointL (l:._)) (PointL (_:.h)) = h - l
  {-# INLINE size #-}
  inBounds (PointL (l:._)) (PointL (_:.h)) (PointL (x:.y)) = l<=x && x<=y && y<=h
  {-# INLINE inBounds #-}

derivingUnbox "PointR"
  [t| PointR -> (Int,Int) |]
  [| \ (PointR (i:.j)) -> (i,j) |]
  [| \ (i,j) -> PointR (i:.j) |]

instance Binary    PointR
instance Serialize PointR
instance FromJSON  PointR
instance ToJSON    PointR

instance Index PointR where
  linearIndex l _ (PointR (z:._)) = undefined
  {-# INLINE linearIndex #-}
  smallestLinearIndex = undefined
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex = undefined
  {-# INLINE largestLinearIndex #-}
  size = undefined
  {-# INLINE size #-}
