
-- | Unit indices admit a single element to be memoized. We can't use @()@
-- because we want to attach phantom types.

module Data.PrimitiveArray.Index.Unit where

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Vector.Fusion.Stream.Monadic (Step(..), map)
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import Prelude hiding (map)
import Test.QuickCheck (Arbitrary(..), choose)

import Data.PrimitiveArray.Index.Class



data Unit t = Unit
  deriving (Eq,Ord,Show,Generic,Read)

derivingUnbox "Unit"
  [t| forall t . Unit t -> () |]
  [| \ Unit -> ()   |]
  [| \ ()   -> Unit |]

instance Binary    (Unit t)
instance Serialize (Unit t)
instance FromJSON  (Unit t)
instance ToJSON    (Unit t)
instance Hashable  (Unit t)

instance NFData (Unit t) where
  rnf Unit = ()
  {-# Inline rnf #-}

instance Index (Unit t) where
  linearIndex _ _ _ = 0
  {-# Inline linearIndex #-}
  smallestLinearIndex _ = 0
  {-# Inline smallestLinearIndex #-}
  largestLinearIndex _ = 0
  {-# Inline largestLinearIndex #-}
  size _ _ = 1
  {-# Inline size #-}
  inBounds _ _ _ = True
  {-# Inline inBounds #-}

instance IndexStream z => IndexStream (z:.Unit t) where
  streamUp (ls:.Unit) (hs:.Unit) = map (\z -> z:.Unit) $ streamUp ls hs
  {-# Inline streamUp #-}
  streamDown (ls:.Unit) (hs:.Unit) = map (\z -> z:.Unit) $ streamDown ls hs
  {-# Inline streamDown #-}

instance (IndexStream (Z:.Unit t)) => IndexStream (Unit t)

instance Arbitrary (Unit t) where
  arbitrary = pure Unit
  shrink Unit = []
