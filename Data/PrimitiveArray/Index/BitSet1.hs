
-- | A bitset with one interface. This includes the often-encountered case
-- where @{u,v},{v}@, or sets with a single edge between the old set and a new
-- singleton set are required. Uses are Hamiltonian path problems, and TSP,
-- among others.

module Data.PrimitiveArray.Index.BitSet1 where

import           Control.DeepSeq (NFData(..))
import           Control.Lens (makeLenses)
import           Control.Monad.Except
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
import           Test.QuickCheck

import           Data.Bits.Ordered
import           Data.PrimitiveArray.Index.BitSet0 (BitSet(..),LimitType(..))
import           Data.PrimitiveArray.Index.BitSetClasses
import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC



-- | The bitset with one interface or boundary.

data BitSet1 i ioc = BitSet1 { _bitset ∷ !(BitSet ioc), _boundary ∷ !(Boundary i ioc) }
  deriving (Eq,Ord,Generic,Show)
makeLenses ''BitSet1

derivingUnbox "BitSet1"
  [t| forall i ioc . BitSet1 i ioc → (Int,Int)           |]
  [| \ (BitSet1 (BitSet set) (Boundary bnd)) → (set,bnd) |]
  [| \ (set,bnd) → BitSet1 (BitSet set) (Boundary bnd)   |]


-- |
--
-- NOTE We linearize a bitset as follows: we need @2^number-of-bits *
-- number-of-bits@ elements. The first is due to having a binary set structure.
-- The second is due to pointing to each of those elements as being the
-- boundary. This overcommits on memory since only those bits can be a boundary
-- bits that are actually set. Furthermore, in case no bit is set at all, then
-- there should be no boundary. This is currently rather awkwardly done by
-- restricting enumeration and mapping the 0-set to boundary 0.
--
-- | TODO The size calculations are off by a factor of two, exactly. Each
-- bitset (say) @00110@ has a mirror image @11001@, whose elements do not have
-- to be indexed. It has to be investigated if a version with exact memory
-- bounds is slower in indexing.

instance Index (BitSet1 bnd ioc) where
  newtype LimitType (BitSet1 bnd ioc) = LtBitSet1 Int
  -- Calculate the linear index for a set. Spread out by the possible number of
  -- bits to fit the actual boundary results. Add the boundary index.
  linearIndex (LtBitSet1 pc) (BitSet1 set (Boundary bnd))
    = linearIndex (LtBitSet pc) set * pc + bnd
  {-# Inline linearIndex #-}
  size (LtBitSet1 pc) = 2^pc * pc + 1
  {-# Inline size #-}
  inBounds (LtBitSet1 pc) (BitSet1 set bnd) = popCount set <= pc && 0 <= bnd && getBoundary bnd <= pc
  {-# Inline inBounds #-}
  zeroBound = BitSet1 zeroBound zeroBound
  {-# Inline zeroBound #-}
  zeroBound' = LtBitSet1 0
  {-# Inline zeroBound' #-}
  totalSize (LtBitSet1 pc) =
    let z = fromIntegral pc
    in  [z * 2 ^ z]

deriving instance Show (LimitType (BitSet1 bnd ioc))

instance IndexStream z ⇒ IndexStream (z:.BitSet1 i I) where
  streamUp   (ls:..LtBitSet1 l) (hs:..LtBitSet1 h) = SM.flatten (streamUpMk   l h) (streamUpStep   l h) $ streamUp   ls hs
  streamDown (ls:..LtBitSet1 l) (hs:..LtBitSet1 h) = SM.flatten (streamDownMk l h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

instance IndexStream z ⇒ IndexStream (z:.BitSet1 i O) where
  streamUp   (ls:..LtBitSet1 l) (hs:..LtBitSet1 h) = SM.flatten (streamDownMk l h) (streamDownStep l h) $ streamUp   ls hs
  streamDown (ls:..LtBitSet1 l) (hs:..LtBitSet1 h) = SM.flatten (streamUpMk   l h) (streamUpStep   l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

--instance IndexStream z => IndexStream (z:.BS1 i C) where
--  streamUp   (ls:..l) (hs:..h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamUp   ls hs
--  streamDown (ls:..l) (hs:..h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}

instance IndexStream (Z:.BitSet1 i t) ⇒ IndexStream (BitSet1 i t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# Inline streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# Inline streamDown #-}

streamUpMk ∷ Monad m ⇒ Int → Int → z → m (z, Maybe (BitSet1 c ioc))
streamUpMk l h z =
  let set = BitSet $ 2^l-1
      -- lsbZ set == 0, or no active bits in which case we use 0
      bnd = Boundary 0
  in  return (z, if l <= h then Just (BitSet1 set bnd) else Nothing)
{-# Inline [0] streamUpMk #-}

streamUpStep ∷ Monad m ⇒ Int → Int → (t, Maybe (BitSet1 c ioc)) → m (SM.Step (t, Maybe (BitSet1 c ioc)) (t:.BitSet1 c ioc))
streamUpStep l h (z , Nothing) = return $ SM.Done
streamUpStep l h (z,  Just t ) = return $ SM.Yield (z:.t) (z , setSucc (2^l-1) (2^h-1) t)
{-# Inline [0] streamUpStep #-}

streamDownMk ∷ Monad m ⇒ Int → Int → z → m (z, Maybe (BitSet1 c ioc))
streamDownMk l h z =
  let set = BitSet $ 2^h-1
      bnd = Boundary 0
  in  return (z, if l <= h then Just (BitSet1 set bnd) else Nothing)
{-# Inline [0] streamDownMk #-}

streamDownStep ∷ Monad m ⇒ Int → Int → (t, Maybe (BitSet1 c ioc)) → m (SM.Step (t, Maybe (BitSet1 c ioc)) (t:.BitSet1 c ioc))
streamDownStep l h (z , Nothing) = return $ SM.Done
streamDownStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
{-# Inline [0] streamDownStep #-}

instance SetPredSucc (BitSet1 t ioc) where
  setSucc pcl pch (BitSet1 s (Boundary is))
    | cs > pch                         = Nothing
    | Just is' <- maybeNextActive is s = Just $ BitSet1 s  (Boundary is')
    | Just s'  <- popPermutation pch s = Just $ BitSet1 s' (Boundary $ lsbZ s')
    | cs >= pch                        = Nothing
    | cs < pch                         = let s' = BitSet $ 2^(cs+1)-1
                                         in  Just (BitSet1 s' (Boundary (lsbZ s')))
    where cs = popCount s
  {-# Inline setSucc #-}
  setPred pcl pch (BitSet1 s (Boundary is))
    | cs < pcl                          = Nothing
    | Just is' <- maybeNextActive is s  = Just $ BitSet1 s  (Boundary is')
    | Just s'  <- popPermutation pch s  = Just $ BitSet1 s' (Boundary $ lsbZ s')
    | cs <= pcl                         = Nothing
    | cs > pcl                          = let s' = BitSet $ 2^(cs-1)-1
                                          in  Just (BitSet1 s' (Boundary (max 0 $ lsbZ s')))
    where cs = popCount s
  {-# Inline setPred #-}

instance Arbitrary (BitSet1 t ioc) where
  arbitrary = do
    s <- arbitrary
    if s==0
      then return (BitSet1 s 0)
      else do i <- elements $ activeBitsL s
              return (BitSet1 s $ Boundary i)
  shrink (BitSet1 s i) =
    let s' = [ BitSet1 (s `clearBit` a) i
             | a <- activeBitsL s
             , Boundary a /= i ]
             ++ [ BitSet1 0 0 | popCount s == 1 ]
    in  s' ++ concatMap shrink s'

