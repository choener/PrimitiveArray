
-- | A linear 0-based int-index with a phantom type.

module Data.PrimitiveArray.Index.PhantomInt where

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Data
import Data.Ix(Ix)
import Data.Serialize (Serialize)
import Data.Typeable
import Data.Vector.Fusion.Stream.Monadic (flatten,map,Step(..))
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import Prelude hiding (map)

import Data.PrimitiveArray.Index.Class



-- | A 'PInt' behaves exactly like an @Int@, but has an attached phantom
-- type @p@. In particular, the @Index@ and @IndexStream@ instances are the
-- same as for raw @Int@s.

newtype PInt p = PInt { getPInt :: Int }
  deriving (Read,Show,Eq,Ord,Enum,Num,Integral,Real,Generic,Data,Typeable,Ix)


derivingUnbox "PInt"
  [t| forall p . PInt p -> Int |]  [| getPInt |]  [| PInt |]

instance Binary    (PInt p)
instance Serialize (PInt p)
instance FromJSON  (PInt p)
instance ToJSON    (PInt p)

instance NFData (PInt p)

instance Index (PInt p) where
  linearIndex _ _ (PInt k) = k
  {-# Inline linearIndex #-}
  smallestLinearIndex _ = error "still needed?"
  {-# Inline smallestLinearIndex #-}
  largestLinearIndex (PInt h) = h
  {-# Inline largestLinearIndex #-}
  size _ (PInt h) = h+1
  {-# Inline size #-}
  inBounds l h k = l <= k && k <= h
  {-# Inline inBounds #-}

instance IndexStream z => IndexStream (z:.(PInt p)) where
  streamUp (ls:.l) (hs:.h) = flatten mk step Unknown $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ Done
            | otherwise = return $ Yield (z:.k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.l) (hs:.h) = flatten mk step Unknown $ streamDown ls hs
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

