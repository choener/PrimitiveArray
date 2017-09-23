
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
import Data.Vector.Fusion.Stream.Monadic (map,Step(..))
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import Prelude hiding (map)

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Vector.Compat



-- | A 'PInt' behaves exactly like an @Int@, but has an attached phantom
-- type @p@. In particular, the @Index@ and @IndexStream@ instances are the
-- same as for raw @Int@s.

newtype PInt (t ∷ k) (p ∷ k) = PInt { getPInt :: Int }
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
  type UpperLimit (PInt t p) = Int
  linearIndex _ (PInt k) = k
  {-# Inline linearIndex #-}
  size _ h = h+1
  {-# Inline size #-}
  inBounds h (PInt k) = 0 <= k && k <= h
  {-# Inline inBounds #-}

instance IndexStream z => IndexStream (z:.PInt I p) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpMk   l h) (streamUpStep   l h) $ streamUp ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownMk l h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.PInt O p) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamDownMk l h) (streamDownStep l h) $ streamUp ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamUpMk   l h) (streamUpStep   l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

instance IndexStream z => IndexStream (z:.PInt C p) where
  streamUp   (ls:.l) (hs:.h) = flatten (streamUpMk   l h) (streamUpStep   l h) $ streamUp ls hs
  streamDown (ls:.l) (hs:.h) = flatten (streamDownMk l h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp   #-}
  {-# Inline streamDown #-}

streamUpMk l h z = return (z,l)
{-# Inline [0] streamUpMk #-}

streamUpStep l h (z,k)
  | k > h     = return $ Done
  | otherwise = return $ Yield (z:.k) (z,k+1)
{-# Inline [0] streamUpStep #-}

streamDownMk l h z = return (z,h)
{-# Inline [0] streamDownMk #-}

streamDownStep l h (z,k)
  | k < l     = return $ Done
  | otherwise = return $ Yield (z:.k) (z,k-1)
{-# Inline [0] streamDownStep #-}

instance IndexStream (PInt I p) where
  streamUp l h = map (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}
  streamDown l h = map (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}

instance IndexStream (PInt O p) where
  streamUp l h = map (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}
  streamDown l h = map (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}

instance IndexStream (PInt C p) where
  streamUp l h = map (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}
  streamDown l h = map (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}

{-
instance IndexStream z => IndexStream (z:.(PInt p)) where
  streamUp (ls:.l) (hs:.h) = flatten mk step $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ Done
            | otherwise = return $ Yield (z:.k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.l) (hs:.h) = flatten mk step $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ Done
            | otherwise = return $ Yield (z:.k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream (PInt p) where
  streamUp l h = map (\(Z:.k) -> k) $ streamUp (Z:.l) (Z:.h)
  {-# Inline streamUp #-}
  streamDown l h = map (\(Z:.k) -> k) $ streamDown (Z:.l) (Z:.h)
  {-# Inline streamDown #-}
-}

