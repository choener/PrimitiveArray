
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
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           Debug.Trace
import           GHC.Generics (Generic)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck (Arbitrary(..), choose, elements)

import           Data.Bits.Ordered
import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC
import           Data.PrimitiveArray.Vector.Compat



-- * @newtype@s, @data@ types, @class@es.



-- | Certain sets have an interface, a particular element with special
-- meaning. In this module, certain ``meanings'' are already provided.
-- These include a @First@ element and a @Last@ element. We phantom-type
-- these to reduce programming overhead.

newtype Interface t = Iter { getIter :: Int }
  deriving (Eq,Ord,Generic,Num)

instance Show (Interface t) where
  show (Iter i) = "(I:" ++ show i ++ ")"

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

newtype BitSet t = BitSet { getBitSet :: Int }
  deriving (Eq,Ord,Read,Generic,FiniteBits,Ranked,Num,Bits)

bitSetI :: Int -> BitSet I
bitSetI = BitSet
{-# Inline bitSetI #-}

bitSetO :: Int -> BitSet O
bitSetO = BitSet
{-# Inline bitSetO #-}

bitSetC :: Int -> BitSet C
bitSetC = BitSet
{-# Inline bitSetC #-}

-- | A bitset with one interface.

-- type BS1 t i = BitSet t :> Interface i

data BS1 i t = BS1 !(BitSet t) !(Interface i)

deriving instance Show (BS1 i t)

-- | A bitset with two interfaces.

-- type BS2 t i j = BitSet t :> Interface i :> Interface j

data BS2 i j t = BS2 !(BitSet t) !(Interface i) !(Interface j)

deriving instance Show (BS2 i j t)

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
  [t| forall t . BitSet t -> Int |]
  [| \(BitSet s) -> s   |]
  [| BitSet             |]

instance Show (BitSet t) where
  show (BitSet s) = "<" ++ (show $ activeBitsL s) ++ ">(" ++ show s ++ ")"

instance Binary    (BitSet t)
instance Serialize (BitSet t)
instance ToJSON    (BitSet t)
instance FromJSON  (BitSet t)
instance Hashable  (BitSet t)

instance NFData (BitSet t) where
  rnf (BitSet s) = rnf s
  {-# Inline rnf #-}

instance Index (BitSet t) where
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

instance IndexStream z => IndexStream (z:.BitSet I) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsMk   l h) (streamUpBsStep   l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsMk l h) (streamDownBsStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.BitSet O) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamDownBsMk l h) (streamDownBsStep l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamUpBsMk   l h) (streamUpBsStep   l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.BitSet C) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsMk   l h) (streamUpBsStep   l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsMk l h) (streamDownBsStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream (Z:.BitSet t) => IndexStream (BitSet t)

streamUpBsMk l h z = return (z, if l <= h then Just l else Nothing)
{-# Inline [0] streamUpBsMk #-}

streamUpBsStep l h (z , Nothing) = return $ SM.Done
streamUpBsStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
{-# Inline [0] streamUpBsStep #-}

streamDownBsMk l h z = return (z, if l <=h then Just h else Nothing)
{-# Inline [0] streamDownBsMk #-}

streamDownBsStep l h (z , Nothing) = return $ SM.Done
streamDownBsStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
{-# Inline [0] streamDownBsStep #-}



-- ** @BS1@

instance IndexStream z => IndexStream (z:.BS1 i I) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.BS1 i O) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.BS1 i C) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream (Z:.BS1 i t) => IndexStream (BS1 i t)

streamUpBsIMk (BS1 sl _) (BS1 sh _) z = return (z, if sl <= sh then Just (BS1 sl (Iter . max 0 $ lsbZ sl)) else Nothing)
{-# Inline [0] streamUpBsIMk #-}

streamUpBsIStep l h (z , Nothing) = return $ SM.Done
streamUpBsIStep l h (z,  Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
{-# Inline [0] streamUpBsIStep #-}

streamDownBsIMk (BS1 sl _) (BS1 sh _) z = return (z, if sl <= sh then Just (BS1 sl (Iter . max 0 $ lsbZ sh)) else Nothing)
{-# Inline [0] streamDownBsIMk #-}

streamDownBsIStep l h (z , Nothing) = return $ SM.Done
streamDownBsIStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
{-# Inline [0] streamDownBsIStep #-}



instance IndexStream z => IndexStream (z:.BS2 i j I) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsIiMk   l h) (streamUpBsIiStep   l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIiMk l h) (streamDownBsIiStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.BS2 i j O) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamDownBsIiMk l h) (streamDownBsIiStep l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamUpBsIiMk   l h) (streamUpBsIiStep   l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.BS2 i j C) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsIiMk   l h) (streamUpBsIiStep   l h) $ streamUp   ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIiMk l h) (streamDownBsIiStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream (Z:.BS2 i j t) => IndexStream (BS2 i j t)

streamUpBsIiMk (BS2 sl _ _) (BS2 sh _ _) z
  | sl > sh   = return (z , Nothing)
  | cl == 0   = return (z , Just (BS2 0 0 0))
  | cl == 1   = let i = lsbZ sl
                in  return (z , Just (BS2 sl (Iter i) (Iter i)))
  | otherwise = let i = lsbZ sl; j = lsbZ (sl `clearBit` i)
                in  return (z , Just (BS2 sl (Iter i) (Iter j)))
  where cl = popCount sl
{-# Inline [0] streamUpBsIiMk #-}

streamUpBsIiStep l h (z , Nothing) = return $ SM.Done
streamUpBsIiStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
{-# Inline [0] streamUpBsIiStep #-}

streamDownBsIiMk (BS2 sl _ _) (BS2 sh _ _) z
  | sl > sh   = return (z , Nothing)
  | ch == 0   = return (z , Just (BS2 0 0 0))
  | ch == 1   = let i = lsbZ sh
                in  return (z , Just (BS2 sh (Iter i) (Iter i)))
  | otherwise = let i = lsbZ sh; j = lsbZ sh
                in  return (z , Just (BS2 sh (Iter i) (Iter j)))
  where ch = popCount sh
{-# Inline [0] streamDownBsIiMk #-}

streamDownBsIiStep l h (z , Nothing) = return $ SM.Done
streamDownBsIiStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
{-# Inline [0] streamDownBsIiStep #-}



-- ** Set predecessor and successor

instance SetPredSucc (BitSet t) where
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

instance SetPredSucc (BS1 i t) where
  setSucc (BS1 l il) (BS1 h ih) (BS1 s (Iter is))
    | cs > ch                          = Nothing
    | Just is' <- maybeNextActive is s = Just $ BS1 s  (Iter is')
    | Just s'  <- popPermutation ch s  = Just $ BS1 s' (Iter $ lsbZ s')
    | cs >= ch                         = Nothing
    | cs < ch                          = let s' = BitSet $ 2^(cs+1)-1 in Just (BS1 s' (Iter (lsbZ s')))
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (BS1 l il) (BS1 h ih) (BS1 s (Iter is))
    | cs < cl                          = Nothing
    | Just is' <- maybeNextActive is s = Just $ BS1 s  (Iter is')
    | Just s'  <- popPermutation ch s  = Just $ BS1 s' (Iter  $ lsbZ s')
    | cs <= cl                         = Nothing
    | cs > cl                          = let s' = BitSet $ 2^(cs-1)-1 in Just (BS1 s' (Iter (max 0 $ lsbZ s')))
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}

instance SetPredSucc (BS2 i j t) where
  setSucc (BS2 l il jl) (BS2 h ih jh) (BS2 s (Iter is) (Iter js))
    -- early termination
    | cs > ch                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Just (BS2 1 0 0)
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbZ s'          = Just (BS2 s' (Iter is') (Iter is'))
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- maybeNextActive js (s `clearBit` is) = Just (BS2 s (Iter is) (Iter js'))
    -- advance other interface, 
    | Just is' <- maybeNextActive is s
    , let js' = lsbZ (s `clearBit` is')      = Just (BS2 s (Iter is') (Iter js'))
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Iter is') (Iter js'))
    -- increasing the population forbidden by upper limit
    | cs >= ch                        = Nothing
    -- increase population
    | cs < ch
    , let s' = BitSet $ 2^(cs+1)-1
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Iter is') (Iter js'))
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (BS2 l il jl) (BS2 h ih jh) (BS2 s (Iter is) (Iter js))
    -- early termination
    | cs < cl                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Nothing
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbZ s'          = Just (BS2 s' (Iter is') (Iter is'))
    -- return the single @0@ set
    | cs == 1                         = Just (BS2 0 0 0)
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- maybeNextActive js (s `clearBit` is) = Just (BS2 s (Iter is) (Iter js'))
    -- advance other interface, 
    | Just is' <- maybeNextActive is s
    , let js' = lsbZ (s `clearBit` is')      = Just (BS2 s (Iter is') (Iter js'))
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Iter is') (Iter js'))
    -- decreasing the population forbidden by upper limit
    | cs <= cl                        = Nothing
    -- decrease population
    | cs > cl && cs > 2
    , let s' = BitSet $ 2^(cs-1)-1
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Iter is') (Iter js'))
    -- decrease population to single-element sets
    | cs > cl && cs == 2              = Just (BS2 1 0 0)
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}



type instance Mask (BitSet t) = BitSet t

type instance Mask (BS1 i t)  = BitSet t

type instance Mask (BitSet t :> Interface i :> Interface j) = BitSet t



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

testBsS :: BitSet t -> Maybe (Fixed (BitSet t))
testBsS k = setSucc (Fixed 0 0) (Fixed 0 7) (Fixed 4 k)
{-# NoInline testBsS #-}

instance SetPredSucc (Fixed (BitSet t)) where
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

instance SetPredSucc (Fixed (BS1 i t)) where
  setPred (Fixed _ (BS1 l li)) (Fixed _ (BS1 h hi)) (Fixed !m (BS1 s i))
    | s `testBit` getIter i = (Fixed m . (`BS1` i) . ( `setBit` getIter i)) <$> setPred l h (s .&. complement m)
    | otherwise             = (Fixed m) <$> setPred (BS1 l li) (BS1 h hi) (BS1 (s .&. complement m) i)
  {-# Inline setPred #-}
  setSucc (Fixed _ (BS1 l li)) (Fixed _ (BS1 h hi)) (Fixed !m (BS1 s i))
    | s `testBit` getIter i = (Fixed m . (`BS1` i) . ( `setBit` getIter i)) <$> setSucc l h (s .&. complement m)
    | otherwise             = (Fixed m) <$> setSucc (BS1 l li) (BS1 h hi) (BS1 (s .&. complement m) i)
  {-# Inline setSucc #-}

instance SetPredSucc (Fixed (BitSet t:>Interface i:>Interface j)) where
{-
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
-}


instance ApplyMask (BitSet t) where
  applyMask = popShiftL
  {-# Inline applyMask #-}

instance ApplyMask (BS1 i t) where
  applyMask m (BS1 s i)
    | popCount s == 0 = BS1 0 0
    | otherwise       = BS1 (popShiftL m s) (Iter . getBitSet . popShiftL m . BitSet $ 2 ^ getIter i)
  {-# Inline applyMask #-}

instance ApplyMask (BitSet t :> Interface i :> Interface j) where
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

instance Arbitrary (BitSet t) where
  arbitrary = BitSet <$> choose (0,2^arbitraryBitSetMax-1)
  shrink s = let s' = [ s `clearBit` a | a <- activeBitsL s ]
             in  s' ++ concatMap shrink s'

instance Arbitrary (BitSet t :> Interface i) where
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

instance Arbitrary (BitSet t :> Interface i :> Interface j) where
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

