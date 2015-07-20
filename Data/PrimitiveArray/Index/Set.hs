
-- | Set with and without interfaces. We provide instances for sets, and
-- sets with one or two interfaces. The @First@ and @Last@ annotation is
-- purely cosmetical (apart from introducing type safety).

module Data.PrimitiveArray.Index.Set where

import           Control.Applicative ((<$>),(<*>))
import           Control.DeepSeq (NFData(..))
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.Bits
import           Data.Bits.Extras
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize)
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           Debug.Trace
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck (Arbitrary(..), choose, elements)

import           Data.Bits.Ordered
import           Data.PrimitiveArray.Index.Class



-- * @newtype@s, @data@ types, @class@es.



-- | Certain sets have an interface, a particular element with special
-- meaning. In this module, certain ``meanings'' are already provided.
-- These include a @First@ element and a @Last@ element. We phantom-type
-- these to reduce programming overhead.

newtype Interface t = Iter { getIter :: Int }
  deriving (Eq,Ord,Read,Show,Generic,Num)

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

newtype BitSet = BitSet { getBitSet :: Int }
  deriving (Eq,Ord,Read,Generic,FiniteBits,Ranked,Num,Bits)

-- | A bitset with one interface.

type BS1I i = BitSet:>Interface i

-- | A bitset with two interfaces.

type BS2I i j = BitSet:>Interface i:>Interface j

-- | Successor and Predecessor for sets. Designed as a class to accomodate
-- sets with interfaces and without interfaces with one function.
--
-- The functions are not written recursively, as we currently only have
-- three cases, and we do not want to "reset" while generating successors
-- and predecessors.
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

-- | Masks are used quite often for different types of bitsets. We liberate
-- them as a type family.

type family Mask s :: *

-- | @Fixed@ allows us to fix some or all bits of a bitset, thereby
-- providing @succ/pred@ operations which are only partially free.
--
-- The mask is lazy, this allows us to have @undefined@ for @l@ and @h@.
--
-- @f = getFixedMask .&. getFixed@ are the fixed bits.
-- @n = getFixed .&. complement getFixedMask@ are the free bits.
-- @to = complement getFixed@ is the to move mask
-- @n' = popShiftR to n@ yields the population after the move
-- @p = popPermutation undefined n'@ yields the new population permutation
-- @p' = popShiftL to p@ yields the population moved back
-- @final = p' .|. f@

data Fixed t = Fixed { getFixedMask :: (Mask t) , getFixed :: !t }

-- | Assuming a bitset on bits @[0 .. highbit]@, we can apply a mask that
-- stretches out those bits over @[0 .. higherBit]@ with @highbit <=
-- higherBit@. Any active interfaces are correctly set as well.

class ApplyMask s where
  applyMask :: Mask s -> s -> s



-- * Instances



derivingUnbox "Interface"
  [t| forall t . Interface t -> Int |]
  [| \(Iter i) -> i            |]
  [| Iter                      |]

instance Binary    (Interface t)
instance Serialize (Interface t)
instance ToJSON    (Interface t)
instance FromJSON  (Interface t)
instance Hashable  (Interface t)

instance NFData (Interface t) where
  rnf (Iter i) = rnf i
  {-# Inline rnf #-}

instance Index (Interface i) where
  linearIndex l _ (Iter z) = z - smallestLinearIndex l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (Iter l) = l
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (Iter h) = h
  {-# INLINE largestLinearIndex #-}
  size (Iter l) (Iter h) = h - l + 1
  {-# INLINE size #-}
  inBounds l h z = l <= z && z <= h
  {-# INLINE inBounds #-}



derivingUnbox "BitSet"
  [t| BitSet     -> Int |]
  [| \(BitSet s) -> s   |]
  [| BitSet             |]

instance Show BitSet where
  show (BitSet s) = "<" ++ (show $ activeBitsL s) ++ ">(" ++ show s ++ ")"

instance Binary    BitSet
instance Serialize BitSet
instance ToJSON    BitSet
instance FromJSON  BitSet
instance Hashable  BitSet

instance NFData BitSet where
  rnf (BitSet s) = rnf s
  {-# Inline rnf #-}

instance Index BitSet where
  linearIndex l _ (BitSet z) = z - smallestLinearIndex l -- (2 ^ popCount l - 1)
  {-# INLINE linearIndex #-}
  smallestLinearIndex l = 2 ^ popCount l - 1
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex h = 2 ^ popCount h - 1
  {-# INLINE largestLinearIndex #-}
  size l h = 2 ^ popCount h - 2 ^ popCount l + 1
  {-# INLINE size #-}
  inBounds l h z = popCount l <= popCount z && popCount z <= popCount h
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
    where mk z = return (z, (if sl<=sh then Just (sl:>(Iter . max 0 $ lsbZ sl)) else Nothing))
          step (z , Nothing) = return $ SM.Done
          step (z,  Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.l@(sl:>_)) (hs:.h@(sh:>_)) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = return (z, (if sl<=sh then Just (sh:>(Iter . max 0 $ lsbZ sh)) else Nothing))
          step (z , Nothing) = return $ SM.Done
          step (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.(BitSet:>Interface i:>Interface j)) where
  streamUp (ls:.l@(sl:>_:>_)) (hs:.h@(sh:>_:>_)) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z | sl > sh   = return (z , Nothing)
               | cl == 0   = return (z , Just (0:>0:>0))
               | cl == 1   = let i = lsbZ sl
                             in  return (z , Just (sl :> Iter i :> Iter i))
               | otherwise = let i = lsbZ sl; j = lsbZ (sl `clearBit` i)
                             in  return (z , Just (sl :> Iter i :> Iter j))
               where cl = popCount sl
          step (z , Nothing) = return $ SM.Done
          step (z , Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.l@(sl:>_:>_)) (hs:.h@(sh:>_:>_)) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z | sl > sh   = return (z , Nothing)
               | ch == 0   = return (z , Just (0:>0:>0))
               | ch == 1   = let i = lsbZ sh
                             in  return (z , Just (sh :> Iter i :> Iter i))
               | otherwise = let i = lsbZ sh; j = lsbZ sh
                             in  return (z , Just (sh :> Iter i :> Iter j))
               where ch = popCount sh
          step (z , Nothing) = return $ SM.Done
          step (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}



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
  setSucc (l:>il) (h:>ih) (s:>Iter is)
    | cs > ch                         = Nothing
    | Just is' <- maybeNextActive is s     = Just (s:>Iter is')
    | Just s'  <- popPermutation ch s = Just (s':>Iter (lsbZ s'))
    | cs >= ch                        = Nothing
    | cs < ch                         = let s' = BitSet $ 2^(cs+1)-1 in Just (s' :> Iter (lsbZ s'))
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (l:>il) (h:>ih) (s:>Iter is)
    | cs < cl                         = Nothing
    | Just is' <- maybeNextActive is s     = Just (s:>Iter is')
    | Just s'  <- popPermutation ch s = Just (s':>Iter (lsbZ s'))
    | cs <= cl                        = Nothing
    | cs > cl                         = let s' = BitSet $ 2^(cs-1)-1 in Just (s' :> Iter (max 0 $ lsbZ s'))
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}

instance SetPredSucc (BitSet:>Interface i:>Interface j) where
  setSucc (l:>il:>jl) (h:>ih:>jh) (s:>Iter is:>Iter js)
    -- early termination
    | cs > ch                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Just (1:>0:>0)
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbZ s'          = Just (s':>Iter is':>Iter is')
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- maybeNextActive js (s `clearBit` is) = Just (s:>Iter is:>Iter js')
    -- advance other interface, 
    | Just is' <- maybeNextActive is s
    , let js' = lsbZ (s `clearBit` is')      = Just (s:>Iter is':>Iter js')
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (s':>Iter is':>Iter js')
    -- increasing the population forbidden by upper limit
    | cs >= ch                        = Nothing
    -- increase population
    | cs < ch
    , let s' = BitSet $ 2^(cs+1)-1
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (s':>Iter is':>Iter js')
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (l:>il:>jl) (h:>ih:>jh) (s:>Iter is:>Iter js)
    -- early termination
    | cs < cl                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Nothing
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbZ s'          = Just (s':>Iter is':>Iter is')
    -- return the single @0@ set
    | cs == 1                         = Just (0:>0:>0)
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- maybeNextActive js (s `clearBit` is) = Just (s:>Iter is:>Iter js')
    -- advance other interface, 
    | Just is' <- maybeNextActive is s
    , let js' = lsbZ (s `clearBit` is')      = Just (s:>Iter is':>Iter js')
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (s':>Iter is':>Iter js')
    -- decreasing the population forbidden by upper limit
    | cs <= cl                        = Nothing
    -- decrease population
    | cs > cl && cs > 2
    , let s' = BitSet $ 2^(cs-1)-1
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (s':>Iter is':>Iter js')
    -- decrease population to single-element sets
    | cs > cl && cs == 2              = Just (1:>0:>0)
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}



type instance Mask BitSet = BitSet

type instance Mask (BitSet :> Interface i) = BitSet

type instance Mask (BitSet :> Interface i :> Interface j) = BitSet



derivingUnbox "Fixed"
  [t| forall t . (Unbox t, Unbox (Mask t)) => Fixed t -> (Mask t, t) |]
  [| \(Fixed m s) -> (m,s)              |]
  [| uncurry Fixed                      |]

deriving instance (Eq t     , Eq      (Mask t)) => Eq      (Fixed t)
deriving instance (Ord t    , Ord     (Mask t)) => Ord     (Fixed t)
deriving instance (Read t   , Read    (Mask t)) => Read    (Fixed t)
deriving instance (Show t   , Show    (Mask t)) => Show    (Fixed t)
deriving instance (Generic t, Generic (Mask t)) => Generic (Fixed t)
instance (Generic t, Generic (Mask t), Hashable t, Hashable (Mask t)) => Hashable (Fixed t)

instance (Generic t, Generic (Mask t), Binary t   , Binary    (Mask t)) => Binary    (Fixed t)
instance (Generic t, Generic (Mask t), Serialize t, Serialize (Mask t)) => Serialize (Fixed t)

instance NFData (Fixed t) where
  rnf (Fixed m s) = m `seq` s `seq` ()

-- TODO we need to be careful here, that we actually fix all bits that are
-- fixed AND that during permutations / increases in popCount we do not set
-- an already fixed bit -- as otherwise we lose one in popCount.

testBsS :: BitSet -> Maybe (Fixed BitSet)
testBsS k = setSucc (Fixed 0 0) (Fixed 0 7) (Fixed 4 k)
{-# NoInline testBsS #-}

instance SetPredSucc (Fixed BitSet) where
  setPred (Fixed _ l) (Fixed _ h) (Fixed !m s) = Fixed m <$> setPred l h (s .&. complement m)
  {-# Inline setPred #-}
  --setSucc (Fixed _ l) (Fixed _ h) (Fixed !m s) = Fixed m <$> setSucc l h (s .&. complement m)
  --setSucc (Fixed _ l) (Fixed _ h) (Fixed !m' s) = (Fixed m . (.|. f)) <$> p -- return population, now again including the fixed part @f@
  --  where m = m' .&. h            -- constrain the mask to just the bits until @h@
  --        f = s .&. m             -- these bits are fixed to @1@
  --        n = s .&. complement m  -- these bits are free to be @0@ or @1@ and may move around; this means that @n `subset` complement m@
  --        to = complement m       -- once we have calculated our permutation, we move it to the correct places via @to@
  --        n' = popShiftR to n     -- population without holes. all primes denote that we are in hole-free space.
  --        p' = popPermutation (popCount $ h .&. to) n'  -- permutate the shifted population
  --        p  = popShiftL to <$> p'  -- undo the shift
  setSucc (Fixed _ l) (Fixed _ h) (Fixed !m' s) = traceShow (h,m,s,' ',fb0,fb1,' ',p',p'',p) $ (Fixed m . (.|. fb1)) <$> p
    where m   = m' .&. h
          fb0 = m  .&. complement s
          fb1 = m  .&. s
          p'  = popShiftR m s
          p'' = setSucc (popShiftR m l) (popShiftR m h) p'
          p   = popShiftL m <$> p''
  {-# Inline setSucc #-}

instance SetPredSucc (Fixed (BitSet:>Interface i)) where
  setPred (Fixed _ (l:>li)) (Fixed _ (h:>hi)) (Fixed !m (s:>i))
    | s `testBit` getIter i = (Fixed m . (:> i) . ( `setBit` getIter i)) <$> setPred l h (s .&. complement m)
    | otherwise             = (Fixed m) <$> setPred (l:>li) (h:>hi) ((s .&. complement m):>i)
  {-# Inline setPred #-}
  setSucc (Fixed _ (l:>li)) (Fixed _ (h:>hi)) (Fixed !m (s:>i))
    | s `testBit` getIter i = (Fixed m . (:> i) . ( `setBit` getIter i)) <$> setSucc l h (s .&. complement m)
    | otherwise             = (Fixed m) <$> setSucc (l:>li) (h:>hi) ((s .&. complement m):>i)
  {-# Inline setSucc #-}

instance SetPredSucc (Fixed (BitSet:>Interface i:>Interface j)) where
  setPred (Fixed _ (l:>li:>lj)) (Fixed _ (h:>hi:>hj)) (Fixed !m (s:>i:>j))
    | s `testBit` getIter i && s `testBit` getIter j
    = (Fixed m . (\z       -> (z `setBit` getIter i `setBit` getIter j:>i:>j ))) <$> setPred l h (s .&. complement m)
    | s `testBit` getIter i
    = (Fixed m . (\(z:>j') -> (z `setBit` getIter i                   :>i:>j'))) <$> setPred (l:>lj) (h:>hj) (s .&. complement m :>j)
    | s `testBit` getIter j
    = (Fixed m . (\(z:>i') -> (z `setBit` getIter j                   :>i':>j))) <$> setPred (l:>li) (h:>hi) (s .&. complement m :>i)
  {-# Inline setPred #-}
  setSucc (Fixed _ (l:>li:>lj)) (Fixed _ (h:>hi:>hj)) (Fixed !m (s:>i:>j))
    | s `testBit` getIter i && s `testBit` getIter j
    = (Fixed m . (\z       -> (z `setBit` getIter i `setBit` getIter j:>i:>j ))) <$> setSucc l h (s .&. complement m)
    | s `testBit` getIter i
    = (Fixed m . (\(z:>j') -> (z `setBit` getIter i                   :>i:>j'))) <$> setSucc (l:>lj) (h:>hj) (s .&. complement m :>j)
    | s `testBit` getIter j
    = (Fixed m . (\(z:>i') -> (z `setBit` getIter j                   :>i':>j))) <$> setSucc (l:>li) (h:>hi) (s .&. complement m :>i)
  {-# Inline setSucc #-}



instance ApplyMask BitSet where
  applyMask = popShiftL
  {-# Inline applyMask #-}

instance ApplyMask (BitSet :> Interface i) where
  applyMask m (s:>i)
    | popCount s == 0 = 0:>0
    | otherwise       = popShiftL m s :> (Iter . getBitSet . popShiftL m . BitSet $ 2 ^ getIter i)
  {-# Inline applyMask #-}

instance ApplyMask (BitSet :> Interface i :> Interface j) where
  applyMask m (s:>i:>j)
    | popCount s == 0 = 0:>0:>0
    | popCount s == 1 = s' :> i' :> Iter (getIter i')
    | otherwise       = s' :> i' :> j'
    where s' = popShiftL m s
          i' = Iter . getBitSet . popShiftL m . BitSet $ 2 ^ getIter i
          j' = Iter . getBitSet . popShiftL m . BitSet $ 2 ^ getIter j
  {-# Inline applyMask #-}



arbitraryBitSetMax = 6

instance (Arbitrary t, Arbitrary (Mask t)) => Arbitrary (Fixed t) where
  arbitrary = Fixed <$> arbitrary <*> arbitrary
  shrink (Fixed m s) = [ Fixed m' s' | m' <- shrink m, s' <- shrink s ]

instance Arbitrary BitSet where
  arbitrary = BitSet <$> choose (0,2^arbitraryBitSetMax-1)
  shrink s = let s' = [ s `clearBit` a | a <- activeBitsL s ]
             in  s' ++ concatMap shrink s'

instance Arbitrary (BitSet:>Interface i) where
  arbitrary = do
    s <- arbitrary
    if s==0
      then return (s:>Iter 0)
      else do i <- elements $ activeBitsL s
              return (s:>Iter i)
  shrink (s:>i) =
    let s' = [ (s `clearBit` a:>i)
             | a <- activeBitsL s
             , Iter a /= i ]
             ++ [ 0 :> Iter 0 | popCount s == 1 ]
    in  s' ++ concatMap shrink s'

instance Arbitrary (BitSet:>Interface i:>Interface j) where
  arbitrary = do
    s <- arbitrary
    case (popCount s) of
      0 -> return (s:>Iter 0:>Iter 0)
      1 -> do i <- elements $ activeBitsL s
              return (s:>Iter i:>Iter i)
      _ -> do i <- elements $ activeBitsL s
              j <- elements $ activeBitsL (s `clearBit` i)
              return (s:>Iter i:>Iter j)
  shrink (s:>i:>j) =
    let s' = [ (s `clearBit` a:>i:>j)
             | a <- activeBitsL s
             , Iter a /= i, Iter a /= j ]
             ++ [ 0 `setBit` a :> Iter a :> Iter a
                | popCount s == 2
                , a <- activeBitsL s ]
             ++ [ 0 :> Iter 0 :> Iter 0
                | popCount s == 1 ]
    in  s' ++ concatMap shrink s'

