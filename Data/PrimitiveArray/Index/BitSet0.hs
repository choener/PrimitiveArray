
-- | The most basic bitset structure. Alone, not particularly useful, because
-- two sets @{u,v},{v',w}@ have no way of annotating the connection between the
-- sets. Together with boundaries this yields sets for useful DP algorithms.

module Data.PrimitiveArray.Index.BitSet0 where

import           Control.DeepSeq (NFData(..))
import           Control.Lens (makeLenses)
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
import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC
import           Data.PrimitiveArray.Index.BitSetClasses



-- | Newtype for a bitset.
--
-- @Int@ integrates better with the rest of the framework. But we should
-- consider moving to @Word@-based indexing, if possible.

newtype BitSet t = BitSet { _bitSet :: Int }
  deriving stock (Eq,Ord,Generic)
  deriving newtype (FiniteBits,Ranked,Num,Bits)
makeLenses ''BitSet

instance FromJSON     (BitSet t)
instance FromJSONKey  (BitSet t)
instance ToJSON       (BitSet t)
instance ToJSONKey    (BitSet t)
instance Binary       (BitSet t)
instance Serialize    (BitSet t)
instance Hashable     (BitSet t)

derivingUnbox "BitSet"
  [t| forall t . BitSet t → Int |]
  [| \(BitSet s) → s            |]
  [| BitSet                     |]

instance Show (BitSet t) where
  show (BitSet s) = "<" ++ (show $ activeBitsL s) ++ ">(" ++ show s ++ ")"

instance NFData (BitSet t) where
  rnf (BitSet s) = rnf s
  {-# Inline rnf #-}

instance Index (BitSet t) where
  newtype LimitType (BitSet t) = LtBitSet Int
  linearIndex _ (BitSet z) = z
  {-# Inline linearIndex #-}
  size (LtBitSet pc) = 2 ^ pc -- 2 ^ popCount h - 2 ^ popCount l + 1
  {-# Inline size #-}
  inBounds (LtBitSet h) z = popCount z <= h
  {-# Inline inBounds #-}
  zeroBound = BitSet 0
  {-# Inline zeroBound #-}
  zeroBound' = LtBitSet 0
  {-# Inline zeroBound' #-}
  totalSize (LtBitSet n) = [2 ^ fromIntegral n]
  {-# Inline totalSize #-}
  fromLinearIndex _ = BitSet
  {-# Inline [0] fromLinearIndex #-}
  showBound (LtBitSet b) = ["LtBitSet " ++ show b]
  showIndex (BitSet b) = ["BitSet " ++ show b]

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

instance IndexStream z => IndexStream (z:.BitSet I) where
  streamUp   (ls:..LtBitSet l) (hs:..LtBitSet h) = SM.flatten (streamUpMk   l h) (streamUpStep   l h) $ streamUp   ls hs
  streamDown (ls:..LtBitSet l) (hs:..LtBitSet h) = SM.flatten (streamDownMk l h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z ⇒ IndexStream (z:.BitSet O) where
  streamUp   (ls:..LtBitSet l) (hs:..LtBitSet h) = SM.flatten (streamDownMk l h) (streamDownStep l h) $ streamUp   ls hs
  streamDown (ls:..LtBitSet l) (hs:..LtBitSet h) = SM.flatten (streamUpMk   l h) (streamUpStep   l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z ⇒ IndexStream (z:.BitSet C) where
  streamUp   (ls:..LtBitSet l) (hs:..LtBitSet h) = SM.flatten (streamUpMk   l h) (streamUpStep   l h) $ streamUp   ls hs
  streamDown (ls:..LtBitSet l) (hs:..LtBitSet h) = SM.flatten (streamDownMk l h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream (Z:.BitSet t) ⇒ IndexStream (BitSet t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# Inline streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# Inline streamDown #-}

streamUpMk ∷ Monad m ⇒ Int → Int → t → m (t, Maybe (BitSet ioc))
streamUpMk l h z = return (z, if l <= h then Just (BitSet $ 2^l-1) else Nothing)
{-# Inline [0] streamUpMk #-}

streamUpStep ∷ Monad m ⇒ Int → Int → (t, Maybe (BitSet ioc)) → m (SM.Step (t, Maybe (BitSet ioc)) (t:.BitSet ioc))
streamUpStep l h (z , Nothing) = return $ SM.Done
streamUpStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z, setSucc (2^l-1) (2^h-1) t)
{-# Inline [0] streamUpStep #-}

streamDownMk ∷ Monad m ⇒ Int → Int → t → m (t, Maybe (BitSet ioc))
streamDownMk l h z = return (z, if l <=h then Just (BitSet $ 2^l-1) else Nothing)
{-# Inline [0] streamDownMk #-}

streamDownStep ∷ Monad m ⇒ Int → Int → (t, Maybe (BitSet ioc)) → m (SM.Step (t, Maybe (BitSet ioc)) (t:.BitSet ioc))
streamDownStep l h (z , Nothing) = return $ SM.Done
streamDownStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred (2^l-1) (2^h-1) t)
{-# Inline [0] streamDownStep #-}

instance Arbitrary (BitSet t) where
  arbitrary = BitSet <$> choose (0,2^arbitraryBitSetMax-1)
  shrink s = let s' = [ s `clearBit` a | a <- activeBitsL s ]
             in  s' ++ concatMap shrink s'

