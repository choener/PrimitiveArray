
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Data.PrimitiveArray.Index.Outside where

import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics

import           Data.PrimitiveArray.Index.Class



newtype Outside z = O { unO :: z }
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "Outside"
  [t| forall z . Unbox z => Outside z -> z |]
  [| unO |]
  [| O   |]

instance Binary    z => Binary    (Outside z)
instance Serialize z => Serialize (Outside z)
instance ToJSON    z => ToJSON    (Outside z)
instance FromJSON  z => FromJSON  (Outside z)

instance Index i => Index (Outside i) where
  linearIndex l h (O i) = linearIndex l h i
  {-# INLINE linearIndex #-}
  smallestLinearIndex (O i) = smallestLinearIndex i
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (O i) = largestLinearIndex i
  {-# INLINE largestLinearIndex #-}
  streamUp (O l) (O h) = fmap O $ streamDown l h
  {-# INLINE streamUp #-}
  streamDown (O l) (O h) = fmap O $ streamUp l h
  {-# INLINE streamDown #-}

