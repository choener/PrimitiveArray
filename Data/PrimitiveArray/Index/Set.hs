
-- | Set with and without interfaces. We provide instances for sets, and
-- sets with one or two interfaces. The @First@ and @Last@ annotation is
-- purely cosmetical (apart from introducing type safety).

module Data.PrimitiveArray.Index.Set where

import           Control.Applicative ((<$>),(<*>))
import           Control.DeepSeq (NFData(..))
import           Data.Aeson (FromJSON,ToJSON,FromJSONKey,ToJSONKey)
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



-- | A bitset with two interfaces.

-- type BS2 t i j = BitSet t :> Boundary i :> Boundary j

data BS2 i j t = BS2 !(BitSet t) !(Boundary i t) !(Boundary j t)

deriving instance Show (BS2 i j t)



-- * Instances







---- ** BS2
--
--instance Index (BS2 i j t) where
--  newtype LimitType (BS2 i j t) = LtBS2 Int
----  linearIndex (BS2 ls li lj) (BS2 hs hi hj) (BS2 s i j) = linearIndex (ls:.li:.lj) (hs:.hi:.hj) (s:.i:.j)
----  {-# INLINE linearIndex #-}
----  size (BS2 ls li lj) (BS2 hs hi hj) = size (ls:.li:.lj) (hs:.hi:.hj)
----  {-# INLINE size #-}
----  inBounds (BS2 ls li lj) (BS2 hs hi hj) (BS2 s i j) = inBounds (ls:.li:.lj) (hs:.hi:.hj) (s:.i:.j)
----  {-# INLINE inBounds #-}
--
--instance IndexStream z => IndexStream (z:.BS2 i j I) where
--  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsIiMk   l h) (streamUpBsIiStep   l h) $ streamUp   ls hs
--  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIiMk l h) (streamDownBsIiStep l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}
--
--instance IndexStream z => IndexStream (z:.BS2 i j O) where
--  streamUp   (ls:.l) (hs:.h) = flatten (streamDownBsIiMk l h) (streamDownBsIiStep l h) $ streamUp   ls hs
--  streamDown (ls:.l) (hs:.h) = flatten (streamUpBsIiMk   l h) (streamUpBsIiStep   l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}
--
--instance IndexStream z => IndexStream (z:.BS2 i j C) where
--  streamUp   (ls:.l) (hs:.h) = flatten (streamUpBsIiMk   l h) (streamUpBsIiStep   l h) $ streamUp   ls hs
--  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIiMk l h) (streamDownBsIiStep l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}
--
--instance IndexStream (Z:.BS2 i j t) => IndexStream (BS2 i j t)
--
--streamUpBsIiMk :: (Monad m) => BS2 a b i -> BS2 c d i -> z -> m (z, Maybe (BS2 e f i))
--streamUpBsIiMk (BS2 sl _ _) (BS2 sh _ _) z
--  | sl > sh   = return (z , Nothing)
--  | cl == 0   = return (z , Just (BS2 0 0 0))
--  | cl == 1   = let i = lsbZ sl
--                in  return (z , Just (BS2 sl (Boundary i) (Boundary i)))
--  | otherwise = let i = lsbZ sl; j = lsbZ (sl `clearBit` i)
--                in  return (z , Just (BS2 sl (Boundary i) (Boundary j)))
--  where cl = popCount sl
--{-# Inline [0] streamUpBsIiMk #-}
--
--streamUpBsIiStep :: (Monad m, SetPredSucc s) => s -> s -> (t, Maybe s) -> m (SM.Step (t, Maybe s) (t :. s))
--streamUpBsIiStep l h (z , Nothing) = return $ SM.Done
--streamUpBsIiStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
--{-# Inline [0] streamUpBsIiStep #-}
--
--streamDownBsIiMk :: (Monad m) => BS2 a b i -> BS2 c d i -> z -> m (z, Maybe (BS2 e f i))
--streamDownBsIiMk (BS2 sl _ _) (BS2 sh _ _) z
--  | sl > sh   = return (z , Nothing)
--  | ch == 0   = return (z , Just (BS2 0 0 0))
--  | ch == 1   = let i = lsbZ sh
--                in  return (z , Just (BS2 sh (Boundary i) (Boundary i)))
--  | otherwise = let i = lsbZ sh; j = lsbZ sh
--                in  return (z , Just (BS2 sh (Boundary i) (Boundary j)))
--  where ch = popCount sh
--{-# Inline [0] streamDownBsIiMk #-}
--
--streamDownBsIiStep :: (Monad m, SetPredSucc s) => s -> s -> (t, Maybe s) -> m (SM.Step (t, Maybe s) (t :. s))
--streamDownBsIiStep l h (z , Nothing) = return $ SM.Done
--streamDownBsIiStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
--{-# Inline [0] streamDownBsIiStep #-}



-- ** Set predecessor and successor

instance SetPredSucc (BS1 i t) where
  setSucc (BS1 l il) (BS1 h ih) (BS1 s (Boundary is))
    | cs > ch                          = Nothing
    | Just is' <- maybeNextActive is s = Just $ BS1 s  (Boundary is')
    | Just s'  <- popPermutation ch s  = Just $ BS1 s' (Boundary $ lsbZ s')
    | cs >= ch                         = Nothing
    | cs < ch                          = let s' = BitSet $ 2^(cs+1)-1 in Just (BS1 s' (Boundary (lsbZ s')))
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (BS1 l il) (BS1 h ih) (BS1 s (Boundary is))
    | cs < cl                          = Nothing
    | Just is' <- maybeNextActive is s = Just $ BS1 s  (Boundary is')
    | Just s'  <- popPermutation ch s  = Just $ BS1 s' (Boundary  $ lsbZ s')
    | cs <= cl                         = Nothing
    | cs > cl                          = let s' = BitSet $ 2^(cs-1)-1 in Just (BS1 s' (Boundary (max 0 $ lsbZ s')))
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}

instance SetPredSucc (BS2 i j t) where
  setSucc (BS2 l il jl) (BS2 h ih jh) (BS2 s (Boundary is) (Boundary js))
    -- early termination
    | cs > ch                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Just (BS2 1 0 0)
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbZ s'          = Just (BS2 s' (Boundary is') (Boundary is'))
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- maybeNextActive js (s `clearBit` is) = Just (BS2 s (Boundary is) (Boundary js'))
    -- advance other interface, 
    | Just is' <- maybeNextActive is s
    , let js' = lsbZ (s `clearBit` is')      = Just (BS2 s (Boundary is') (Boundary js'))
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Boundary is') (Boundary js'))
    -- increasing the population forbidden by upper limit
    | cs >= ch                        = Nothing
    -- increase population
    | cs < ch
    , let s' = BitSet $ 2^(cs+1)-1
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Boundary is') (Boundary js'))
    where ch = popCount h
          cs = popCount s
  {-# Inline setSucc #-}
  setPred (BS2 l il jl) (BS2 h ih jh) (BS2 s (Boundary is) (Boundary js))
    -- early termination
    | cs < cl                         = Nothing
    -- in case nothing was set, set initial set @1@ with both interfaces
    -- pointing to the same element
    | cs == 0                         = Nothing
    -- when only a single element is set, we just permute the population
    -- and set the single interface
    | cs == 1
    , Just s'  <- popPermutation ch s
    , let is' = lsbZ s'          = Just (BS2 s' (Boundary is') (Boundary is'))
    -- return the single @0@ set
    | cs == 1                         = Just (BS2 0 0 0)
    -- try advancing only one of the interfaces, doesn't collide with @is@
    | Just js' <- maybeNextActive js (s `clearBit` is) = Just (BS2 s (Boundary is) (Boundary js'))
    -- advance other interface, 
    | Just is' <- maybeNextActive is s
    , let js' = lsbZ (s `clearBit` is')      = Just (BS2 s (Boundary is') (Boundary js'))
    -- find another permutation of the population
    | Just s'  <- popPermutation ch s
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Boundary is') (Boundary js'))
    -- decreasing the population forbidden by upper limit
    | cs <= cl                        = Nothing
    -- decrease population
    | cs > cl && cs > 2
    , let s' = BitSet $ 2^(cs-1)-1
    , let is' = lsbZ s'
    , Just js' <- maybeNextActive is' s'   = Just (BS2 s' (Boundary is') (Boundary js'))
    -- decrease population to single-element sets
    | cs > cl && cs == 2              = Just (BS2 1 0 0)
    where cl = popCount l
          ch = popCount h
          cs = popCount s
  {-# Inline setPred #-}



type instance Mask (BitSet t)  = BitSet t

type instance Mask (BS1 i t)   = BitSet t

type instance Mask (BS2 i j t) = BitSet t



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
    | s `testBit` getBoundary i = (Fixed m . (`BS1` i) . ( `setBit` getBoundary i)) <$> setPred l h (s .&. complement m)
    | otherwise             = (Fixed m) <$> setPred (BS1 l li) (BS1 h hi) (BS1 (s .&. complement m) i)
  {-# Inline setPred #-}
  setSucc (Fixed _ (BS1 l li)) (Fixed _ (BS1 h hi)) (Fixed !m (BS1 s i))
    | s `testBit` getBoundary i = (Fixed m . (`BS1` i) . ( `setBit` getBoundary i)) <$> setSucc l h (s .&. complement m)
    | otherwise             = (Fixed m) <$> setSucc (BS1 l li) (BS1 h hi) (BS1 (s .&. complement m) i)
  {-# Inline setSucc #-}

instance SetPredSucc (Fixed (BS2 i j t)) where
  setPred (Fixed _ (BS2 l li lj)) (Fixed _ (BS2 h hi hj)) (Fixed !m (BS2 s i j))
    | s `testBit` getBoundary i && s `testBit` getBoundary j
    = (Fixed m . (\z       -> BS2 (z `setBit` getBoundary i `setBit` getBoundary j) i j)) <$> setPred l h (s .&. complement m)
    | s `testBit` getBoundary i
    = (Fixed m . (\(BS1 z j') -> BS2 (z `setBit` getBoundary i) i j')) <$> setPred (BS1 l lj) (BS1 h hj) (BS1 (s .&. complement m) j)
    | s `testBit` getBoundary j
    = (Fixed m . (\(BS1 z i') -> BS2 (z `setBit` getBoundary j) i' j)) <$> setPred (BS1 l li) (BS1 h hi) (BS1 (s .&. complement m) i)
  {-# Inline setPred #-}
  setSucc (Fixed _ (BS2 l li lj)) (Fixed _ (BS2 h hi hj)) (Fixed !m (BS2 s i j))
    | s `testBit` getBoundary i && s `testBit` getBoundary j
    = (Fixed m . (\z       -> BS2 (z `setBit` getBoundary i `setBit` getBoundary j) i j)) <$> setSucc l h (s .&. complement m)
    | s `testBit` getBoundary i
    = (Fixed m . (\(BS1 z j') -> BS2 (z `setBit` getBoundary i) i j')) <$> setSucc (BS1 l lj) (BS1 h hj) (BS1 (s .&. complement m) j)
    | s `testBit` getBoundary j
    = (Fixed m . (\(BS1 z i') -> BS2 (z `setBit` getBoundary j) i' j)) <$> setSucc (BS1 l li) (BS1 h hi) (BS1 (s .&. complement m) i)
  {-# Inline setSucc #-}



instance ApplyMask (BitSet t) where
  applyMask = popShiftL
  {-# Inline applyMask #-}

instance ApplyMask (BS1 i t) where
  applyMask m (BS1 s i)
    | popCount s == 0 = BS1 0 0
    | otherwise       = BS1 (popShiftL m s) (Boundary . lsbZ . popShiftL m . BitSet . bit $ getBoundary i)
  {-# Inline applyMask #-}

instance ApplyMask (BS2 i j t) where
  applyMask m (BS2 s i j)
    | popCount s == 0 = BS2 0 0 0
    | popCount s == 1 = BS2 s' i' (Boundary $ getBoundary i')
    | otherwise       = BS2 s' i' j'
    where s' = popShiftL m s
          i' = Boundary . getBitSet . popShiftL m $ (BitSet $ 2 ^ getBoundary i :: BitSet t)
          j' = Boundary . getBitSet . popShiftL m $ (BitSet $ 2 ^ getBoundary j :: BitSet t)
  {-# Inline applyMask #-}



arbitraryBitSetMax = 6

instance (Arbitrary t, Arbitrary (Mask t)) => Arbitrary (Fixed t) where
  arbitrary = Fixed <$> arbitrary <*> arbitrary
  shrink (Fixed m s) = [ Fixed m' s' | m' <- shrink m, s' <- shrink s ]

instance Arbitrary (BitSet t) where
  arbitrary = BitSet <$> choose (0,2^arbitraryBitSetMax-1)
  shrink s = let s' = [ s `clearBit` a | a <- activeBitsL s ]
             in  s' ++ concatMap shrink s'

instance Arbitrary (BS1 i t) where
  arbitrary = do
    s <- arbitrary
    if s==0
      then return (BS1 s 0)
      else do i <- elements $ activeBitsL s
              return (BS1 s $ Boundary i)
  shrink (BS1 s i) =
    let s' = [ BS1 (s `clearBit` a) i
             | a <- activeBitsL s
             , Boundary a /= i ]
             ++ [ BS1 0 0 | popCount s == 1 ]
    in  s' ++ concatMap shrink s'

instance Arbitrary (BS2 i j t) where
  arbitrary = do
    s <- arbitrary
    case (popCount s) of
      0 -> return (BS2 s 0 0)
      1 -> do i <- elements $ activeBitsL s
              return (BS2 s (Boundary i) (Boundary i))
      _ -> do i <- elements $ activeBitsL s
              j <- elements $ activeBitsL (s `clearBit` i)
              return (BS2 s (Boundary i) (Boundary j))
  shrink (BS2 s i j) =
    let s' = [ BS2 (s `clearBit` a) i j
             | a <- activeBitsL s
             , Boundary a /= i, Boundary a /= j ]
             ++ [ BS2 (0 `setBit` a) (Boundary a) (Boundary a)
                | popCount s == 2
                , a <- activeBitsL s ]
             ++ [ BS2 0 0 0
                | popCount s == 1 ]
    in  s' ++ concatMap shrink s'

