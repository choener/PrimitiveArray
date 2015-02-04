
{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language EmptyDataDecls #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Data.PrimitiveArray.Index.Set where

import           Data.Aeson
import           Data.Binary
import           Data.Bits
import           Data.Bits.Extras (Ranked)
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU

import           Data.PrimitiveArray.Index.Class
import           Data.Bits.Ordered



-- | Certain sets have an interface, a particular element with special
-- meaning. In this module, certain ``meanings'' are already provided.
-- These include a @First@ element and a @Last@ element. We phantom-type
-- these to reduce programming overhead.

newtype Interface t = Interface Int
  deriving (Eq,Ord,Read,Show,Generic)

derivingUnbox "Interface"
  [t| forall t . Interface t -> Int |]
  [| \(Interface i) -> i            |]
  [| Interface                      |]

data First

data Last

-- | Newtype for a bitset. We'd use @Word@s but that requires more shape
-- instances.

newtype BitSet = BitSet Int
  deriving (Eq,Ord,Read,Show,Generic,FiniteBits,Ranked,Num,Bits)

instance Binary    BitSet
instance Serialize BitSet
instance ToJSON    BitSet
instance FromJSON  BitSet

derivingUnbox "BitSet"
  [t| BitSet     -> Int |]
  [| \(BitSet s) -> s   |]
  [| BitSet             |]

instance Index z => Index (z:.BitSet) where
  linearIndex (ls:.l) (hs:.h) (zs:.BitSet z) = linearIndex ls hs zs * (h+1) + z - l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (z:.BitSet s) = undefined
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex _ = undefined
  {-# INLINE largestLinearIndex #-}
  streamUp (ls:.BitSet l) (hs:.BitSet h) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk i = return (i,2^(popCount l) -1)
          step (i,k)
            | k > fin   = return $ SM.Done
            | otherwise = return $ SM.Yield (i:.BitSet (VU.unsafeIndex v k)) (i,k+1)
          !fin = 2^(popCount h) -1
          !v   = popCntMemoInt (popCount h)
          {-# INLINE [0] mk #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  streamDown (ls:.BitSet l) (hs:.BitSet h) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk i = return (i,2^(popCount h) -1)
          step (i,k)
            | k < fin   = return $ SM.Done
            | otherwise = return $ SM.Yield (i:.BitSet (VU.unsafeIndex v k)) (i,k-1)
          !fin = 2^(popCount l) -1
          !v   = popCntMemoInt (popCount h)
          {-# INLINE [0] mk #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamDown #-}

instance Index BitSet

instance Index z => Index (z:.Interface i) where
  linearIndex (ls:.l) (hs:.h) (zs:.Interface i) = linearIndex ls hs zs * (h+1) + i - l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (z:.Interface i) = undefined
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex = undefined
  {-# INLINE largestLinearIndex #-}
  streamUp (ls:.Interface l) (hs:.Interface h) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.Interface k) (z,k+1)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  streamDown (ls:.Interface l) (hs:.Interface h) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.Interface k) (z,k-1)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamDown #-}

