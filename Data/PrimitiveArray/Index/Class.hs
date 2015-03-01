
{-# Language DefaultSignatures #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Data.PrimitiveArray.Index.Class where

import           Control.Applicative
import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Test.QuickCheck



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

deriving instance (Read a, Read b) => Read (a:.b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :. b) where
  arbitrary     = (:.) <$> arbitrary <*> arbitrary
  shrink (a:.b) = (:.) <$> shrink a  <*> shrink b




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

instance Arbitrary Z where
  arbitrary = return Z



{-
type family LH z where
  LH (is:.i) = (LH is :. LH i)
  LH Z       = Z
  LH i       = Int
-}

-- |
--
-- TODO i need two functions: @enumUp :: i -> Stream@, and @flattenUp ::
-- i -> Stream -> Stream@.

class Index i where

  -- | Given a minimal size, a maximal size, and a current index, calculate
  -- the linear index.

  linearIndex :: i -> i -> i -> Int
--  default linearIndex :: (Index (Z:.i), LH (Z:.i) ~ (Z:.LH i)) => LH i -> LH i -> i -> Int
--  linearIndex l h i = linearIndex (Z:.l) (Z:.h) (Z:.i)
--  {-# INLINE linearIndex #-}

  -- | Given an index element from the smallest subset, calculate the
  -- highest linear index that is *not* stored.

  smallestLinearIndex :: i -> Int -- LH i
--  default smallestLinearIndex :: (Index (Z:.i), LH (Z:.i) ~ (Z:.Int)) => i -> Int
--  smallestLinearIndex i =
--    let (Z:.l) = smallestLinearIndex (Z:.i)
--    in  l
--  {-# INLINE smallestLinearIndex #-}

  -- | Given an index element from the largest subset, calculate the
  -- highest linear index that *is* stored.

  largestLinearIndex :: i -> Int -- LH i
--  default largestLinearIndex :: (Index (Z:.i), LH (Z:.i) ~ (Z:.Int)) => i -> Int
--  largestLinearIndex i =
--    let (Z:.h) = largestLinearIndex (Z:.i)
--    in  h
--  {-# INLINE largestLinearIndex #-}

  -- | Given smallest and largest index, return the number of cells
  -- required for storage.

  size :: i -> i -> Int

  -- | Check if an index is within the bounds.

  inBounds :: i -> i -> i -> Bool

-- | Generate a stream of indices in correct order for dynamic programming.
-- Since the stream generators require @concatMap@ / @flatten@ we have to
-- write more specialized code for @(z:.IX)@ stuff.

class IndexStream i where

  -- |

  streamUp   :: Monad m => i -> i -> Stream m i
  default streamUp :: (Monad m, IndexStream (Z:.i)) => i -> i -> Stream m i
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (Z:.l) (Z:.h)
  {-# INLINE streamUp #-}

  -- |

  streamDown :: Monad m => i -> i -> Stream m i
  default streamDown :: (Monad m, IndexStream (Z:.i)) => i -> i -> Stream m i
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (Z:.l) (Z:.h)
  {-# INLINE streamDown #-}



instance Index Z where
--  type LH Z = Z
  linearIndex _ _ _ = 0
  {-# INLINE linearIndex #-}
  smallestLinearIndex _ = 0
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex _ = 0
  {-# INLINE largestLinearIndex #-}
  size _ _ = 1
  {-# INLINE size #-}
  inBounds _ _ _ = True
  {-# INLINE inBounds #-}

instance IndexStream Z where
  streamUp   Z Z = SM.singleton Z
  {-# INLINE streamUp #-}
  streamDown Z Z = SM.singleton Z
  {-# INLINE streamDown #-}

instance (Index zs, Index z) => Index (zs:.z) where
  linearIndex (ls:.l) (hs:.h) (zs:.z) = linearIndex ls hs zs * (largestLinearIndex h + 1) + linearIndex l h z
  {-# INLINE linearIndex #-}
  smallestLinearIndex (ls:.l) = smallestLinearIndex ls * smallestLinearIndex l
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (hs:.h) = largestLinearIndex hs * largestLinearIndex h
  {-# INLINE largestLinearIndex #-}
  size (ls:.l) (hs:.h) = size ls hs * (size l h)
  {-# INLINE size #-}
  inBounds (ls:.l) (hs:.h) (zs:.z) = inBounds ls hs zs && inBounds l h z
  {-# INLINE inBounds #-}



infixl 3 :>
data a :> b = !a :> !b
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "StrictIxPair"
  [t| forall a b . (Unbox a, Unbox b) => (a:>b) -> (a,b) |]
  [| \(a:>b) -> (a, b) |]
  [| \(a,b)  -> (a:>b) |]

instance (Binary    a, Binary    b) => Binary    (a:>b)
instance (Serialize a, Serialize b) => Serialize (a:>b)
instance (ToJSON    a, ToJSON    b) => ToJSON    (a:>b)
instance (FromJSON  a, FromJSON  b) => FromJSON  (a:>b)

deriving instance (Read a, Read b) => Read (a:>b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :> b) where
  arbitrary = (:>) <$> arbitrary <*> arbitrary
  shrink (a:>b) = (:>) <$> shrink a <*> shrink b

-- The current implementation for inductive tuples is not efficient. We would
-- like to be able to generate index-streams for tree-like indices. An example
-- is @( (Set:.Interface) :. (Set:.Interface) )@.
--
-- TODO: isn't this just @streamUp (is:.i) = flattenUp i $ map (\i -> (i)) $
-- streamUp is@ ???
--
-- With better fusing @concatMap@ we should revisit this

{-
instance (IndexStream a, IndexStream b) => IndexStream (a:.b) where
  streamUp (lis:.li) (his:.hi) = SM.concatMap (\is -> SM.map (\i -> (is:.i)) $ streamUp li hi) $ streamUp lis his
  {-# INLINE streamUp #-}
  streamDown (lis:.li) (his:.hi) = SM.concatMap (\is -> SM.map (\i -> (is:.i)) $ streamDown li hi) $ streamDown lis his
  {-# INLINE streamDown #-}
-}

