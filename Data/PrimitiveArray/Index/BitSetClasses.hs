
-- | A collection of a number of data types and type classes shared by all
-- bitset variants.

module Data.PrimitiveArray.Index.BitSetClasses where

import           Control.DeepSeq (NFData(..))
import           Data.Aeson (FromJSON,ToJSON,FromJSONKey,ToJSONKey)
import           Data.Binary (Binary)
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics (Generic)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU

import           Data.Bits.Ordered
import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC



-- * Boundaries, the interface(s) for bitsets.

-- | Certain sets have an interface, a particular element with special
-- meaning. In this module, certain ``meanings'' are already provided.
-- These include a @First@ element and a @Last@ element. We phantom-type
-- these to reduce programming overhead.

newtype Boundary boundaryType ioc = Boundary { getBoundary ∷ Int }
  deriving (Eq,Ord,Generic,Num)

instance Show (Boundary i t) where
  show (Boundary i) = "(I:" ++ show i ++ ")"

derivingUnbox "Boundary"
  [t| forall i t . Boundary i t → Int |]
  [| \(Boundary i) → i                |]
  [| Boundary                         |]

instance Binary    (Boundary i t)
instance Serialize (Boundary i t)
instance ToJSON    (Boundary i t)
instance FromJSON  (Boundary i t)
instance Hashable  (Boundary i t)

instance NFData (Boundary i t) where
  rnf (Boundary i) = rnf i
  {-# Inline rnf #-}

instance Index (Boundary i t) where
  newtype LimitType (Boundary i t) = LtBoundary Int
  linearIndex _ (Boundary z) = z
  {-# INLINE linearIndex #-}
  size (LtBoundary h) = h + 1
  {-# INLINE size #-}
  inBounds (LtBoundary h) z = 0 <= z && getBoundary z <= h
  {-# INLINE inBounds #-}
  zeroBound = Boundary 0
  {-# Inline zeroBound #-}
  zeroBound' = LtBoundary 0
  {-# Inline zeroBound' #-}
  totalSize (LtBoundary n) = [fromIntegral n]
  {-# Inline totalSize #-}

instance IndexStream z ⇒ IndexStream (z:.Boundary k I) where
  streamUp   (ls:..LtBoundary l) (hs:..LtBoundary h) = SM.flatten (streamUpBndMk   l h) (streamUpBndStep   l h) $ streamUp   ls hs
  streamDown (ls:..LtBoundary l) (hs:..LtBoundary h) = SM.flatten (streamDownBndMk l h) (streamDownBndStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream (Z:.Boundary k I) ⇒ IndexStream (Boundary k I) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# Inline streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# Inline streamDown #-}

streamUpBndMk l h z = return (z, l)
{-# Inline [0] streamUpBndMk #-}

streamUpBndStep l h (z , k)
  | k > h     = return $ SM.Done
  | otherwise = return $ SM.Yield (z:.Boundary k) (z, k+1)
{-# Inline [0] streamUpBndStep #-}

streamDownBndMk l h z = return (z, h)
{-# Inline [0] streamDownBndMk #-}

streamDownBndStep l h (z , k)
  | k < l     = return $ SM.Done
  | otherwise = return $ SM.Yield (z:.Boundary k) (z,k-1)
{-# Inline [0] streamDownBndStep #-}

-- | Declare the interface to be the start of a path.

data First

-- | Declare the interface to be the end of a path.

data Last

-- | Declare the interface to match anything.
--
-- TODO needed? want to use later in ADPfusion

data Any



-- * Moving indices within sets.

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
  setSucc ∷ Int → Int → s → Maybe s
  -- | Set predecessor. The first argument is the lower set limit, the
  -- second the upper set limit, the third the current set.
  setPred ∷ Int → Int → s → Maybe s

-- | Masks are used quite often for different types of bitsets. We liberate
-- them as a type family.

type family Mask s ∷ *

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



-- | for 'Test.QuickCheck.Arbitrary'

arbitraryBitSetMax ∷ Int
arbitraryBitSetMax = 6

