
-- | Unit indices admit a single element to be memoized. We can't use @()@
-- because we want to attach phantom types.

module Data.PrimitiveArray.Index.Unit where

import Control.Applicative (pure)
import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON,FromJSONKey,ToJSON,ToJSONKey)
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

instance Binary       (Unit t)
instance Serialize    (Unit t)
instance FromJSON     (Unit t)
instance FromJSONKey  (Unit t)
instance ToJSON       (Unit t)
instance ToJSONKey    (Unit t)
instance Hashable     (Unit t)

instance NFData (Unit t) where
  rnf Unit = ()
  {-# Inline rnf #-}

instance Index (Unit t) where
  data LimitType (Unit t) = LtUnit
  linearIndex _ _ = 0
  {-# Inline linearIndex #-}
  size _ = 1
  {-# Inline size #-}
  inBounds _ _ = True
  {-# Inline inBounds #-}
  zeroBound = Unit
  {-# Inline zeroBound #-}
  zeroBound' = LtUnit
  {-# Inline zeroBound' #-}

instance IndexStream z => IndexStream (z:.Unit t) where
  streamUp (ls:..LtUnit) (hs:..LtUnit) = map (\z -> z:.Unit) $ streamUp ls hs
  {-# Inline streamUp #-}
  streamDown (ls:..LtUnit) (hs:..LtUnit) = map (\z -> z:.Unit) $ streamDown ls hs
  {-# Inline streamDown #-}

instance (IndexStream (Z:.Unit t)) => IndexStream (Unit t) where
  streamUp l h = map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}

instance Arbitrary (Unit t) where
  arbitrary = pure Unit
  shrink Unit = []

