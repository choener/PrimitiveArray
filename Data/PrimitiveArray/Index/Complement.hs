
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Data.PrimitiveArray.Index.Complement where

import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics
import           Test.QuickCheck
import           Control.Applicative

import           Data.PrimitiveArray.Index.Class



-- |

newtype Complement z = C { unC :: z }
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "Complement"
  [t| forall z . Unbox z => Complement z -> z |]
  [| unC |]
  [| C   |]

instance Binary    z => Binary    (Complement z)
instance Serialize z => Serialize (Complement z)
instance ToJSON    z => ToJSON    (Complement z)
instance FromJSON  z => FromJSON  (Complement z)

instance Index i => Index (Complement i) where
  linearIndex (C l) (C h) (C i) = linearIndex l h i
  {-# INLINE linearIndex #-}
  smallestLinearIndex (C i) = smallestLinearIndex i
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (C i) = largestLinearIndex i
  {-# INLINE largestLinearIndex #-}
  size (C l) (C h) = size l h
  {-# INLINE size #-}
  inBounds (C l) (C h) (C z) = inBounds l h z
  {-# INLINE inBounds #-}

instance IndexStream i => IndexStream (Complement i) where
  streamUp   (C l) (C h) = fmap C $ streamUp l h
  {-# INLINE streamUp #-}
  streamDown (C l) (C h) = fmap C $ streamDown l h
  {-# INLINE streamDown #-}

instance Arbitrary z => Arbitrary (Complement z) where
  arbitrary    = C <$> arbitrary
  shrink (C z) = C <$> shrink z

