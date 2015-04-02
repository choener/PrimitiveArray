
-- | Set with and without interfaces. We provide instances for sets, and
-- sets with one or two interfaces. The @First@ and @Last@ annotation is
-- purely cosmetical (apart from introducing type safety).

module Data.PrimitiveArray.Index.Set where

import           Control.Applicative ((<$>))
import           Control.DeepSeq (NFData(..))
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.Bits
import           Data.Bits.Extras
import           Data.Serialize (Serialize)
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           Debug.Trace
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU

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

instance Binary    (Interface t)
instance Serialize (Interface t)
instance ToJSON    (Interface t)
instance FromJSON  (Interface t)

instance NFData (Interface t) where
  rnf (Interface i) = rnf i
  {-# Inline rnf #-}

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
--
-- TODO can we use @Word@s now?

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

instance NFData BitSet where
  rnf (BitSet s) = rnf s
  {-# Inline rnf #-}

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
    where mk z = return (z , (if l <= h then Just l else Nothing))
          step (z , Nothing) = return $ SM.Done
          step (z , Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp   #-}
  streamDown (ls:.l) (hs:.h) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = return (z :. (if l <= h then Just h else Nothing))
          step (z :. Nothing) = return $ SM.Done
          step (z :. Just t ) = return $ SM.Yield (z:.t) (z :. setPred l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.(BitSet:>Interface i)) where
  streamUp (ls:.l@(sl:>_)) (hs:.h@(sh:>_)) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = return (z, (if sl<=sh then Just (sl:>(Interface $ lsbActive sl)) else Nothing))
          step (z , Nothing) = return $ SM.Done
          step (z,  Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.l@(sl:>_)) (hs:.h@(sh:>_)) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = return (z, (if sl<=sh then Just (sh:>(Interface $ lsbActive sh)) else Nothing))
          step (z , Nothing) = return $ SM.Done
          step (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.(BitSet:>Interface i:>Interface j)) where
  streamUp (ls:.l@(sl:>_:>_)) (hs:.h@(sh:>_:>_)) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z | sl > sh   = return (z , Nothing)
               | cl == 0   = return (z , Just (BitSet 0 :> Interface (-1) :> Interface (-1)))
               | cl == 1   = let i = lsbActive sl
                             in  return (z , Just (sl :> Interface i :> Interface i))
               | otherwise = let i = lsbActive sl; j = lsbActive (sl `clearBit` i)
                             in  return (z , Just (sl :> Interface i :> Interface j))
               where cl = popCount sl
          step (z , Nothing) = return $ SM.Done
          step (z , Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.l@(sl:>_:>_)) (hs:.h@(sh:>_:>_)) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z | sl > sh   = return (z , Nothing)
               | ch == 0   = return (z , Just (BitSet 0 :> Interface (-1) :> Interface (-1)))
               | ch == 1   = let i = lsbActive sh
                             in  return (z , Just (sh :> Interface i :> Interface i))
               | otherwise = let i = lsbActive sh; j = lsbActive sh
                             in  return (z , Just (sh :> Interface i :> Interface j))
               where ch = popCount sh
          step (z , Nothing) = return $ SM.Done
          step (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}



-- | Successor and Predecessor for sets. Designed as a class to accomodate
-- sets with interfaces and without interfaces with one function.
--
-- The functions are not written recursively, as we currently only have
-- three cases, and we not to "reset" while generating successors and
-- predecessors.
--
-- Note that sets have a partial order. Within the group of element with
-- the same @popCount@, we use @popPermutation@ which has the same stepping
-- order for both, @setSucc@ and @setPred@.

class SetPredSucc s where
  -- | Set successor. The first argument is the lower set limit, the second
  -- the upper set limit, the third the current set.
  setSucc :: s -> s -> s -> Maybe s
  -- | Set predecessor. The first argument is the lower set limit, the
  -- second the upper set limit, the third the current set.
  setPred :: s -> s -> s -> Maybe s



instance SetPredSucc BitSet where
  setSucc l h s
    | cs > ch                        = Nothing
    | Just s' <- popPermutation ch s = Just s'
    | cs >= ch                       = Nothing
    | cs < ch                        = Just . BitSet $ 2^(cs+1) -1
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred l h s
    | cs < cl                        = Nothing
    | Just s' <- popPermutation ch s = Just s'
    | cs <= cl                       = Nothing
    | cs > cl                        = Just . BitSet $ 2^(cs-1) -1
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}



instance SetPredSucc (BitSet:>Interface i) where
  setSucc (l:>il) (h:>ih) (s:>Interface is)
    | cs > ch                         = Nothing
    | Just is' <- succActive is s     = Just (s:>Interface is')
    | Just s'  <- popPermutation ch s = Just (s':>Interface (lsbActive s'))
    | cs >= ch                        = Nothing
    | cs < ch                         = let s' = BitSet $ 2^(cs+1)-1 in Just (s' :> Interface (lsbActive s'))
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (l:>il) (h:>ih) (s:>Interface is)
    | cs < cl                         = Nothing
    | Just is' <- succActive is s     = Just (s:>Interface is')
    | Just s'  <- popPermutation ch s = Just (s':>Interface (lsbActive s'))
    | cs <= cl                        = Nothing
    | cs > cl                         = let s' = BitSet $ 2^(cs-1)-1 in Just (s' :> Interface (lsbActive s'))
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}



instance SetPredSucc (BitSet:>Interface i:>Interface j) where
  setSucc (l:>il:>jl) (h:>ih:>jh) (s:>Interface is:>Interface js)
    -- early termination
    | cs > ch                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Just (1:>Interface 0:>Interface 0)
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbActive s'          = Just (s':>Interface is':>Interface is')
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- succActive js (s `clearBit` is) = Just (s:>Interface is:>Interface js')
    -- advance other interface, 
    | Just is' <- succActive is s
    , let js' = lsbActive (s `clearBit` is')      = Just (s:>Interface is':>Interface js')
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbActive s'
    , Just js' <- succActive is' s'   = Just (s':>Interface is':>Interface js')
    -- increasing the population forbidden by upper limit
    | cs >= ch                        = Nothing
    -- increase population
    | cs < ch
    , let s' = BitSet $ 2^(cs+1)-1
    , let is' = lsbActive s'
    , Just js' <- succActive is' s'   = Just (s':>Interface is':>Interface js')
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (l:>il:>jl) (h:>ih:>jh) (s:>Interface is:>Interface js)
    -- early termination
    | cs < cl                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Nothing
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbActive s'          = Just (s':>Interface is':>Interface is')
    -- return the single @0@ set
    | cs == 1                         = Just (0:>Interface (-1):>Interface (-1))
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- succActive js (s `clearBit` is) = Just (s:>Interface is:>Interface js')
    -- advance other interface, 
    | Just is' <- succActive is s
    , let js' = lsbActive (s `clearBit` is')      = Just (s:>Interface is':>Interface js')
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbActive s'
    , Just js' <- succActive is' s'   = Just (s':>Interface is':>Interface js')
    -- decreasing the population forbidden by upper limit
    | cs <= cl                        = Nothing
    -- decrease population
    | cs > cl && cs > 2
    , let s' = BitSet $ 2^(cs-1)-1
    , let is' = lsbActive s'
    , Just js' <- succActive is' s'   = Just (s':>Interface is':>Interface js')
    -- decrease population to single-element sets
    | cs > cl && cs == 2              = Just (1:>Interface 0:>Interface 0)
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}

