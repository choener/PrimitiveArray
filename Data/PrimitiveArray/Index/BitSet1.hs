
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

import           Data.Bits.Ordered
import           Data.PrimitiveArray.Index.BitSet0
import           Data.PrimitiveArray.Index.BitSetClasses
import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC



-- | The bitset with one interface or boundary.

data BitSet1 i ioc = BitSet1 { _bitset ∷ !(BitSet ioc), _boundary ∷ !(Boundary i ioc) }
  deriving (Eq,Ord,Generic,Show)
makeLenses ''BitSet1

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
    if pc < (last $ 0 : activeBitsL (maxBound `div` pc))
    then return . CellSize . fromIntegral $ size (LtBitSet1 pc)
    else throwError $ SizeError "BitSet1 too large!"

{-
instance IndexStream z => IndexStream (z:.BS1 i I) where
  streamUp   (ls:..l) (hs:..h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamUp   ls hs
--  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}

--instance IndexStream z => IndexStream (z:.BS1 i O) where
--  streamUp   (ls:.l) (hs:.h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamUp   ls hs
--  streamDown (ls:.l) (hs:.h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}
--
--instance IndexStream z => IndexStream (z:.BS1 i C) where
--  streamUp   (ls:..l) (hs:..h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamUp   ls hs
--  streamDown (ls:..l) (hs:..h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}

instance IndexStream (Z:.BS1 i t) => IndexStream (BS1 i t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}


--streamUpBsIMk :: (Monad m) => BS1 a i -> BS1 b i -> z -> m (z, Maybe (BS1 c i))
streamUpBsIMk (LtBS1 sl) (LtBS1 sh) z = return (z, if sl <= sh then Just (BS1 sl (Boundary . max 0 $ lsbZ sl)) else Nothing)
{-# Inline [0] streamUpBsIMk #-}

--streamUpBsIStep :: (Monad m, SetPredSucc s) => s -> s -> (t, Maybe s) -> m (SM.Step (t, Maybe s) (t :. s))
streamUpBsIStep = undefined
--streamUpBsIStep l h (z , Nothing) = return $ SM.Done
--streamUpBsIStep l h (z,  Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
{-# Inline [0] streamUpBsIStep #-}

--streamDownBsIMk :: (Monad m) => BS1 a i -> BS1 b i -> z -> m (z, Maybe (BS1 c i))
--streamDownBsIMk (BS1 sl _) (BS1 sh _) z = return (z, if sl <= sh then Just (BS1 sh (Boundary . max 0 $ lsbZ sh)) else Nothing)
--{-# Inline [0] streamDownBsIMk #-}
--
--streamDownBsIStep :: (Monad m, SetPredSucc s) => s -> s -> (t, Maybe s) -> m (SM.Step (t, Maybe s) (t :. s))
--streamDownBsIStep l h (z , Nothing) = return $ SM.Done
--streamDownBsIStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
--{-# Inline [0] streamDownBsIStep #-}
-}

