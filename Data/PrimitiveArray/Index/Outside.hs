
module Data.PrimitiveArray.Index.Outside where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Binary
import Data.Hashable (Hashable)
import Data.Serialize
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox(..))
import GHC.Generics
import Test.QuickCheck

import Data.PrimitiveArray.Index.Class



-- | The 'Outside' wrapper takes an index structure, and provides
-- 'IndexStream' functions 'streamUp' and 'streamDown' that work the other
-- way around. In particular, for @Outside z@ @streamUp (Outside z) = fmap
-- Outside $ streamDown z@ and vice versa. @Index@ functions are unwrapped
-- but otherwise work as before.

newtype Outside z = O { unO :: z }
  deriving (Eq,Ord,Read,Show,Generic)

derivingUnbox "Outside"
  [t| forall z . Unbox z => Outside z -> z |]
  [| unO |]
  [| O   |]

instance Binary    z => Binary    (Outside z)
instance Serialize z => Serialize (Outside z)
instance ToJSON    z => ToJSON    (Outside z)
instance FromJSON  z => FromJSON  (Outside z)
instance Hashable  z => Hashable  (Outside z)

instance NFData z => NFData (Outside z) where
  rnf (O z) = rnf z
  {-# Inline rnf #-}

instance Index i => Index (Outside i) where
  linearIndex (O l) (O h) (O i) = linearIndex l h i
  {-# INLINE linearIndex #-}
  smallestLinearIndex (O i) = smallestLinearIndex i
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (O i) = largestLinearIndex i
  {-# INLINE largestLinearIndex #-}
  size (O l) (O h) = size l h
  {-# INLINE size #-}
  inBounds (O l) (O h) (O z) = inBounds l h z
  {-# INLINE inBounds #-}

instance IndexStream i => IndexStream (Outside i) where
  streamUp (O l) (O h) = fmap O $ streamDown l h
  {-# INLINE streamUp #-}
  streamDown (O l) (O h) = fmap O $ streamUp l h
  {-# INLINE streamDown #-}

instance Arbitrary z => Arbitrary (Outside z) where
  arbitrary = O <$> arbitrary
  shrink (O z) = O <$> shrink z

