
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

-- |
--
-- TODO i need two functions: @enumUp :: i -> Stream@, and @flattenUp ::
-- i -> Stream -> Stream@.

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

class IndexStream i where
  streamUp   :: Monad m => i -> i -> Stream m i
  streamDown :: Monad m => i -> i -> Stream m i



instance Index Z where
  linearIndex _ _ _ = 0
  {-# INLINE linearIndex #-}
  smallestLinearIndex _ = 0
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex _ = 0
  {-# INLINE largestLinearIndex #-}

-- The current implementation for inductive tuples is not efficient. We would
-- like to be able to generate index-streams for tree-like indices. An example
-- is @( (Set:.Interface) :. (Set:.Interface) )@.
--
-- TODO: isn't this just @streamUp (is:.i) = flattenUp i $ map (\i -> (i)) $
-- streamUp is@ ???

instance (IndexStream a, IndexStream b) => IndexStream (a:.b) where
  streamUp (lis:.li) (his:.hi) = SM.concatMap (\is -> SM.map (\i -> (is:.i)) $ streamUp li hi) $ streamUp lis his
  {-# INLINE streamUp #-}
  streamDown (lis:.li) (his:.hi) = SM.concatMap (\is -> SM.map (\i -> (is:.i)) $ streamDown li hi) $ streamDown lis his
  {-# INLINE streamDown #-}

