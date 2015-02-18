
{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language EmptyDataDecls #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PatternGuards #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

-- | Set with and without interfaces. We provide instances for sets, and
-- sets with one or two interfaces. The @First@ and @Last@ annotation is
-- purely cosmetical (apart from introducing type safety).

module Data.PrimitiveArray.Index.Set where

import           Data.Aeson
import           Data.Binary
import           Data.Bits
import           Data.Bits.Extras
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Control.Applicative ((<$>))
import           Debug.Trace

import           Data.Bits.Ordered
import           Data.PrimitiveArray.Index.Class



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

-- | Declare the interface to be the start of a path.

data First

-- | Declare the interface to be the end of a path.

data Last

-- | Declare the interface to match anything.
--
-- TODO needed? want to use later in ADPfusion

data Any

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

-- | A bitset with one interface.

type BS1I i = BitSet:>Interface i

-- | A bitset with two interfaces.

type BS2I i j = BitSet:>Interface i:>Interface j

instance Index BitSet where
  linearIndex l _ (BitSet z) = z - smallestLinearIndex l -- (2 ^ popCount l - 1)
  {-# INLINE linearIndex #-}
  smallestLinearIndex (BitSet l) = 2 ^ popCount l - 1
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (BitSet h) = 2 ^ popCount h - 1
  {-# INLINE largestLinearIndex #-}
  size (BitSet l) (BitSet h) = 2 ^ popCount h - 2 ^ popCount l + 1
  {-# INLINE size #-}
  inBounds (BitSet l) (BitSet h) (BitSet z) = popCount l <= popCount z && popCount z <= popCount h
  {-# INLINE inBounds #-}

instance Index (Interface i) where
  linearIndex l _ (Interface z) = z - smallestLinearIndex l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (Interface l) = l
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (Interface h) = h
  {-# INLINE largestLinearIndex #-}
  size (Interface l) (Interface h) = h - l + 1
  {-# INLINE size #-}
  inBounds (Interface l) (Interface h) (Interface z) = l <= z && z <= h
  {-# INLINE inBounds #-}

instance IndexStream z => IndexStream (z:.BitSet) where
  streamUp (ls:.l) (hs:.h) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = let k = popCount l in return (z,k,Just $ 2^k-1)
          step (z,k,Nothing)
            | k > c     = return $ SM.Done
            | otherwise = return $ SM.Skip (z,k+1,Just $ 2^(k+1)-1)
          step (z,k,Just s)
            | otherwise = return $ SM.Yield (z:.s) (z,k,succPopulation c s)
          !c   = popCount h
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  streamDown (ls:.l) (hs:.h) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = let k = popCount h in return (z,k,Just $ 2^k-1)
          step (z,k,Nothing)
            | k < cl    = return $ SM.Done
            | otherwise = return $ SM.Skip (z,k-1,Just $ 2^(k-1)-1)
          step (z,k,Just s)
            | otherwise = return $ SM.Yield (z:.s) (z,k,succPopulation ch s)
          !cl = popCount l
          !ch = popCount h
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamDown #-}

testBitset :: BitSet -> BitSet -> IO Int
testBitset l h = SM.foldl' (+) 0 $ SM.map (\(Z:.BitSet z) -> z) $ streamUp (Z:.l) (Z:.h)
{-# NOINLINE testBitset #-}

-- TODO with better working @concatMap@ this would look a lot less ugly
--
-- TODO AT WORK
--
-- NOTE low @Interface i@ and high @Interface i@ completely determined by
-- bitset!

instance IndexStream z => IndexStream (z:.(BitSet:>Interface i)) where
  streamUp (ls:.(lb:>_)) (hs:.(hb:>_)) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = let k = popCount lb in return (z,k,Just (2^k-1, 0)) -- start with lowest bit
          step (z,k,_)
            | k > c = return $ SM.Done
          -- increase popcount
          step (z,k,Nothing)
            | otherwise = return $ SM.Skip (z,k+1,Just (2^(k+1)-1,0))
          -- TODO case with increasing interface here
          step (z,k,Just (s,i))
            | i >= 0 = return $ SM.Yield (z:.(s:>Interface i)) (z,k,Just (s,nextActive i s))
          -- next population permutation
          step (z,k,Just (s,_))
            | otherwise = return $ SM.Skip (z,k,s')
            where !s' = (\z -> (z,lsbActive z)) <$> succPopulation c s
          !c   = popCount hb
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  streamDown (ls:.(lb:>_)) (hs:.(hb:>_)) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = let k = popCount hb in return (z,k,Just (2^k-1,0))
          step (z,k,_)
            | k < cl    = return $ SM.Done
          step (z,k,Nothing)
            | otherwise = return $ SM.Skip (z,k-1,Just (2^(k-1)-1,0))
          step (z,k,Just (s,i))
            | i >= 0 = return $ SM.Yield (z:.(s:>Interface i)) (z,k,Just (s,nextActive i s))
          step (z,k,Just (s,i))
            | otherwise = return $ SM.Skip (z,k,s')
            where !s' = (\z -> (z,lsbActive z)) <$> succPopulation ch s
          !cl = popCount lb
          !ch = popCount hb
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamDown #-}

-- TODO 0,0,0 only if we actually want the empty set case
--
-- TODO should also produce all one-bit sets, ONLY here, start and end fall
-- together.

instance (Show z,IndexStream z) => IndexStream (z:.(BitSet:>Interface i:>Interface j)) where
  streamUp (ls:.(lb:>_:>_)) (hs:.(hb:>_:>_)) = SM.flatten mk step Unknown $ streamUp ls hs
    {-
    where mk z = let k = popCount lb; s = 2^k-1 in return (z,k,Just (s, 0,lsbActive (clearBit s 0))) -- start with lowest bits to zero to capture the empty-set case!
          step (z,k,_)
            | k == 0 = return $ SM.Yield (z:.(BitSet 0:>Interface 0:>Interface 0)) (z,1,Nothing)
          step (z,k,_)
            | k > c = return $ SM.Done
          -- increase popcount
          step (z,k,Nothing)
            | otherwise = let s = 2^(k+1)-1 in return $ SM.Skip (z,k+1,Just (s,0,lsbActive (clearBit s 0)))
          -- TODO case with increasing interface here
          step (z,k,Just (s,i,j))
            | i >= 0, j >= 0 = return $ SM.Yield (z:.(s:>Interface i:>Interface j)) (z,k,Just (s,i,nextActive j (clearBit s i)))
          step (z,k,Just (s,i,j)) -- increase the i index by one, reset j to lowest
            | i >= 0 = let i' = nextActive i s in return $ SM.Skip (z,k,Just (s,i',lsbActive (clearBit s i')))
          -- next population permutation
          step (z,k,Just (s,_,_))
            | otherwise = return $ SM.Skip (z,k,s')
            where s' = (\z -> let i = lsbActive z in (z,i,lsbActive (clearBit z i))) <$> succPopulation c s
          !c   = popCount hb
    -}
    where mk z = let k = popCount lb; s = 2^k-1 in return (z,k,Just (s,0,0))
          step (_,k,_)
            | k > c = return $ SM.Done
          step (z,k,Just(s,i,j))
            | k == 0                  = return $ SM.Yield (z:.(s:>Interface 0      :>Interface 0      ))  (z,1,Just(1,0,0))
            | k == 1                  = return $ SM.Yield (z:.(s:>Interface (lsb s):>Interface (lsb s)))  (z,1,s'         )
            | i >= 0, j >= 0, i /= j  = return $ SM.Yield (z:.(s:>Interface i      :>Interface j      ))  (z,k,Just(s,i,nextActive j s))
            | i >= 0, j >= 0          = return $ SM.Skip                                                  (z,k,Just(s,i,nextActive j s))
            | i >= 0                  = return $ SM.Skip                                                  (z,k,Just(s,nextActive i s, lsbActive s))
            | otherwise               = return $ SM.Skip                                                  (z,k,s')
            where s' = (\z -> let i = lsbActive z in (z,i,i)) <$> succPopulation c s
          step (z,k,Nothing)
            | otherwise = let s = 2^(k+1)-1 in return $ SM.Skip (z,k+1,Just (s,0,0))
          !c   = popCount hb
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  -- TODO rewrite to use same system as @streamUp@
  streamDown (ls:.(lb:>_:>_)) (hs:.(hb:>_:>_)) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = let k = ch; s = 2^k-1 in return (z,k,Just (s, 0,lsbActive (clearBit s 0))) -- start with lowest bits to zero to capture the empty-set case!
          step (z,k,_)
            | k == 0, k == cl = return $ SM.Yield (z:.(BitSet 0:>Interface 0:>Interface 0)) (z,k-1,Nothing)
          step (z,k,_)
            | k < cl = return $ SM.Done -- TODO the 0,0,0 if k==0
          -- increase popcount
          step (z,k,Nothing)
            | otherwise = let s = 2^(k-1)-1 in return $ SM.Skip (z,k-1,Just (s,0,lsbActive (clearBit s 0)))
          -- TODO case with increasing interface here
          step (z,k,Just (s,i,j))
            | i >= 0, j >= 0 = return $ SM.Yield (z:.(s:>Interface i:>Interface j)) (z,k,Just (s,i,nextActive j (clearBit s i)))
          step (z,k,Just (s,i,j)) -- increase the i index by one, reset j to lowest
            | i >= 0 = let i' = nextActive i s in return $ SM.Skip (z,k,Just (s,i',lsbActive (clearBit s i')))
          -- next population permutation
          step (z,k,Just (s,_,_))
            | otherwise = return $ SM.Skip (z,k,s')
            where s' = (\z -> let i = lsbActive z in (z,i,lsbActive (clearBit z i))) <$> succPopulation ch s
          !cl  = popCount lb
          !ch  = popCount hb
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamDown #-}

-- | Given a set with boundaries, return the successor. This steps through
-- the whole poset-tree if called often enough. Boundaries may coincide
-- only in case of single-element sets.

succSet :: BS2I i j -> BS2I i j -> Maybe (BS2I i j)
succSet full@(BitSet zb:>Interface zi:>Interface zj) (BitSet b:>Interface i:>Interface j)
  | c==0 && z>0           = Just $ BitSet 1:>Interface 0:>Interface 0
  | c==1 && msbB < msb zb = Just $ BitSet 2^(msbB+1):>Interface (msbB+1):>Interface (msbB+1)
  -- increase j
  | j' >= 0 && j' /= i    = Just $ BitSet b:>Interface i:>Interface j'
  -- increasing j would make it equal to i, so increase twice
  | j' >= 0, j'' >= 0 && j'' /= i  = Just $ BitSet b:>Interface i:>Interface j''
  -- increase i, reset j
  | i' >= 0               = Just $ BitSet b:>Interface i':>Interface (lsb b)
  -- permutate population, restart i,j, still same population count
  | Just bS <- succPopulation (popCount zb) b
  , let iS = lsb bS; jS = nextActive iS bS
  , jS >= 0
                          = Just $ BitSet bS:>Interface iS:>Interface jS
  -- increase population count by 1, reset everything
  | c < z, let bS = 2^(c+1)-1; iS = lsb bS; jS = nextActive iS bS
                          = Just $ BitSet bS:>Interface iS:>Interface jS
  | otherwise             = Nothing
  where c    = popCount b
        z    = popCount zb
        msbB = msb b
        j'   = nextActive j  b
        j''  = nextActive j' b
        i'   = nextActive i b

-- | Predecessor function. Note that sets are partially ordered. This means
-- that @(predSet . succSet /= id)@ in general.

predSet :: BS2I i j -> BS2I i j -> Maybe (BS2I i j)
predSet full@(BitSet zb:>Interface zi:>Interface zj) (BitSet b:>Interface i:>Interface j)
  -- this was the mpty set
  | c == 0 = Nothing
  | c==1 && msbB < msb zb = Just $ BitSet 2^(msbB+1):>Interface (msbB+1):>Interface (msbB+1)
  -- increase j
  | j' >= 0 && j' /= i    = Just $ BitSet b:>Interface i:>Interface j'
  -- increasing j would make it equal to i, so increase twice
  | j' >= 0, j'' >= 0 && j'' /= i  = Just $ BitSet b:>Interface i:>Interface j''
  -- increase i, reset j
  | i' >= 0               = Just $ BitSet b:>Interface i':>Interface (lsb b)
  -- permutate population, restart i,j, still same population count
  | Just bS <- succPopulation (popCount zb) b
  , let iS = lsb bS; jS = nextActive iS bS
  , jS >= 0
                          = Just $ BitSet bS:>Interface iS:>Interface jS
  -- decrease population count by 1, reset everything
  | c > 2, let bS = 2^(c-1)-1; iS = lsb bS; jS = nextActive iS bS
                          = Just $ BitSet bS:>Interface iS:>Interface jS
  -- single-element sets
  | c == 2, let bS = 2^(c-1)-1; iS = lsb bS; jS = iS
                          = Just $ BitSet bS:>Interface iS:>Interface jS
  -- empty set
  | c == 1                = Just $ BitSet 0:>Interface 0:>Interface 0
  | otherwise             = Nothing
  where c    = popCount b
        z    = popCount zb
        msbB = msb b
        j'   = nextActive j  b
        j''  = nextActive j' b
        i'   = nextActive i b

