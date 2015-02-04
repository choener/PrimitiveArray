
{-# Language DefaultSignatures #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Data.PrimitiveArray.Index.Class where

import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM



infixl 3 :.
data a :. b = !a :. !b
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "StrictPair"
  [t| forall a b . (Unbox a, Unbox b) => (a:.b) -> (a,b) |]
  [| \(a:.b) -> (a, b) |]
  [| \(a,b)  -> (a:.b) |]

instance (Binary    a, Binary    b) => Binary    (a:.b)
instance (Serialize a, Serialize b) => Serialize (a:.b)
instance (ToJSON    a, ToJSON    b) => ToJSON    (a:.b)
instance (FromJSON  a, FromJSON  b) => FromJSON  (a:.b)



data Z = Z
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "Z"
  [t| Z -> () |]
  [| const () |]
  [| const Z  |]

instance Binary    Z
instance Serialize Z
instance ToJSON    Z
instance FromJSON  Z



type family LH z where
  LH (is:.i) = (LH is :. Int)
  LH Z       = Z
  LH i       = Int

type A = LH Int

class Index i where

  -- | Given a minimal size, a maximal size, and a current index, calculate
  -- the linear index.

  linearIndex :: LH i -> LH i -> i -> Int
  default linearIndex :: (Index (Z:.i), LH i ~ Int) => LH i -> LH i -> i -> Int
  linearIndex l h i = linearIndex (Z:.l) (Z:.h) (Z:.i)
  {-# INLINE linearIndex #-}

  -- | Given an index element from the smallest subset, calculate the
  -- highest linear index that is *not* stored.

  smallestLinearIndex :: i -> Int
  default smallestLinearIndex :: (Index (Z:.i)) => i -> Int
  smallestLinearIndex i = smallestLinearIndex (Z:.i)
  {-# INLINE smallestLinearIndex #-}

  -- | Given an index element from the largest subset, calculate the
  -- highest linear index that *is* stored.

  largestLinearIndex :: i -> Int
  default largestLinearIndex :: (Index (Z:.i)) => i -> Int
  largestLinearIndex i = largestLinearIndex (Z:.i)
  {-# INLINE largestLinearIndex #-}

  -- | Enumerate all elements from smallest included to largest included
  -- index. The assumption is that all indices that have already been
  -- enumerated are independent of those indices yet to be enumerated. We
  -- do not impose a total order, thereby this is not a "real" enumeration.

  streamUp :: Monad m => i -> i -> Stream m i
  default streamUp :: (Monad m, Index (Z:.i)) => i -> i -> Stream m i
  streamUp l h = fmap (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}

  -- |

  flattenUp :: (forall j . Monad m) => i -> i -> Stream m j -> Stream m i
--  default flattenUp :: (Monad m) => i -> i -> Stream m (Z:.i) -> Stream m (Z:.i)
--  flattenUp l h = undefined

  -- | Semantically the reverse of 'streamUp'. Available as the reverse
  -- operation is not very efficient.

  streamDown :: Monad m => i -> i -> Stream m i
  default streamDown :: (Monad m, Index (Z:.i)) => i -> i -> Stream m i
  streamDown l h = fmap (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}

instance Index Z where
  linearIndex _ _ _ = 0
  {-# INLINE linearIndex #-}
  smallestLinearIndex _ = 0
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex _ = 0
  {-# INLINE largestLinearIndex #-}
  streamUp _ _ = SM.singleton Z
  {-# INLINE streamUp #-}
  streamDown _ _ = SM.singleton Z
  {-# INLINE streamDown #-}

