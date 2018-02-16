
-- | A linear 0-based int-index with a phantom type.

module Data.PrimitiveArray.Index.PhantomInt where

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON,FromJSONKey,ToJSON,ToJSONKey)
import Data.Binary (Binary)
import Data.Data
import Data.Hashable (Hashable)
import Data.Ix(Ix)
import Data.Serialize (Serialize)
import Data.Typeable
import Data.Vector.Fusion.Stream.Monadic (map,Step(..),flatten)
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import Prelude hiding (map)

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Index.IOC



-- | A 'PInt' behaves exactly like an @Int@, but has an attached phantom
-- type @p@. In particular, the @Index@ and @IndexStream@ instances are the
-- same as for raw @Int@s.

newtype PInt (ioc ∷ k) (p ∷ k) = PInt { getPInt :: Int }
  deriving (Read,Show,Eq,Ord,Enum,Num,Integral,Real,Generic,Data,Typeable,Ix)

pIntI :: Int -> PInt I p
pIntI = PInt
{-# Inline pIntI #-}

pIntO :: Int -> PInt O p
pIntO = PInt
{-# Inline pIntO #-}

pIntC :: Int -> PInt C p
pIntC = PInt
{-# Inline pIntC #-}

derivingUnbox "PInt"
  [t| forall t p . PInt t p -> Int |]  [| getPInt |]  [| PInt |]

instance Binary       (PInt t p)
instance Serialize    (PInt t p)
instance FromJSON     (PInt t p)
instance FromJSONKey  (PInt t p)
instance ToJSON       (PInt t p)
instance ToJSONKey    (PInt t p)
instance Hashable     (PInt t p)
instance NFData       (PInt t p)

instance Index (PInt t p) where
  newtype LimitType (PInt t p) = LtPInt Int
  linearIndex _ (PInt k) = k
  {-# Inline linearIndex #-}
  size (LtPInt h) = h+1
  {-# Inline size #-}
  inBounds (LtPInt h) (PInt k) = 0 <= k && k <= h
  {-# Inline inBounds #-}

instance IndexStream z => IndexStream (z:.PInt I p) where
  streamUp   (ls:..LtPInt l) (hs:..LtPInt h) = flatten (streamUpMk   l h) (streamUpStep   l h) $ streamUp ls hs
  streamDown (ls:..LtPInt l) (hs:..LtPInt h) = flatten (streamDownMk l h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.PInt O p) where
  streamUp   (ls:..LtPInt l) (hs:..LtPInt h) = flatten (streamDownMk l h) (streamDownStep l h) $ streamUp ls hs
  streamDown (ls:..LtPInt l) (hs:..LtPInt h) = flatten (streamUpMk   l h) (streamUpStep   l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.PInt C p) where
  streamUp   (ls:..LtPInt l) (hs:..LtPInt h) = flatten (streamUpMk   l h) (streamUpStep   l h) $ streamUp ls hs
  streamDown (ls:..LtPInt l) (hs:..LtPInt h) = flatten (streamDownMk l h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream (Z:.PInt ioc p) => IndexStream (PInt ioc p) where
  streamUp l h = map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}

streamUpMk l h z = return (z,l)
{-# Inline [0] streamUpMk #-}

streamUpStep l h (z,k)
  | k > h     = return $ Done
  | otherwise = return $ Yield (z:.PInt k) (z,k+1)
{-# Inline [0] streamUpStep #-}

streamDownMk l h z = return (z,h)
{-# Inline [0] streamDownMk #-}

streamDownStep l h (z,k)
  | k < l     = return $ Done
  | otherwise = return $ Yield (z:.PInt k) (z,k-1)
{-# Inline [0] streamDownStep #-}

