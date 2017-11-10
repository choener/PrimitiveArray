
-- | Edge boundaries capture edge indexing of the type @From :-> To@, where
-- both @From@ and @To@ are @Int@s. Each such @Int@ gives one of the two
-- nodes between edge exists.

module Data.PrimitiveArray.Index.EdgeBoundary where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.Monad (filterM, guard)
import Data.Aeson (FromJSON,FromJSONKey,ToJSON,ToJSONKey)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Vector.Fusion.Stream.Monadic (Step(..), map)
import Data.Vector.Unboxed.Deriving
import Debug.Trace
import GHC.Generics (Generic)
import Prelude hiding (map)
import Test.QuickCheck (Arbitrary(..), choose)
import Test.SmallCheck.Series as TS

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Vector.Compat



-- | An edge boundary as two @Int@s denoting the edge @From :-> To@.

data EdgeBoundary t = !Int :-> !Int
  deriving (Eq,Ord,Show,Generic,Read)

fromEdgeBoundaryFst :: EdgeBoundary t -> Int
fromEdgeBoundaryFst (i :-> _) = i
{-# Inline fromEdgeBoundaryFst #-}

fromEdgeBoundarySnd :: EdgeBoundary t -> Int
fromEdgeBoundarySnd (_ :-> j) = j
{-# Inline fromEdgeBoundarySnd #-}

derivingUnbox "EdgeBoundary"
  [t| forall t . EdgeBoundary t -> (Int,Int) |]
  [| \ (f :-> t) -> (f,t) |]
  [| \ (f,t) -> (f :-> t) |]

instance Binary       (EdgeBoundary t)
instance Serialize    (EdgeBoundary t)
instance FromJSON     (EdgeBoundary t)
instance FromJSONKey  (EdgeBoundary t)
instance ToJSON       (EdgeBoundary t)
instance ToJSONKey    (EdgeBoundary t)
instance Hashable     (EdgeBoundary t)

instance NFData (EdgeBoundary t) where
  rnf (f :-> t) = f `seq` rnf t
  {-# Inline rnf #-}




instance Index (EdgeBoundary t) where
  type LimitType (EdgeBoundary t) = Int
  linearIndex t (i :-> j) = i * (t+1) + j
  {-# Inline linearIndex #-}
  size _ t = (t+1) * (t+1)
  {-# Inline size #-}
  inBounds t (i :-> j) = 0<=i && i <= t   &&  0 <= j && j<=t
  {-# Inline inBounds #-}

-- | @EdgeBoundary I@ (inside)

instance IndexStream z => IndexStream (z:.EdgeBoundary I) where
  streamUp   (ls:.(l:->_)) (hs:.(_:->h)) = flatten (streamUpMk   l) (streamUpStep   l h) $ streamUp   ls hs
  streamDown (ls:.(l:->_)) (hs:.(_:->h)) = flatten (streamDownMk h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

-- | @EdgeBoundary O@ (outside).
--
-- Note: @streamUp@ really needs to use @streamDownMk@ / @streamDownStep@
-- for the right order of indices!

instance IndexStream z => IndexStream (z:.EdgeBoundary O) where
  streamUp   (ls:.(l:->_)) (hs:.(_:->h)) = flatten (streamDownMk h) (streamDownStep l h) $ streamUp   ls hs
  streamDown (ls:.(l:->_)) (hs:.(_:->h)) = flatten (streamUpMk   l) (streamUpStep   l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

-- | @EdgeBoundary C@ (complement)

instance IndexStream z => IndexStream (z:.EdgeBoundary C) where
  streamUp   (ls:.(l:->_)) (hs:.(_:->h)) = flatten (streamUpMk   l) (streamUpStep   l h) $ streamUp   ls hs
  streamDown (ls:.(l:->_)) (hs:.(_:->h)) = flatten (streamDownMk h) (streamDownStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

-- | generic @mk@ for @streamUp@ / @streamDown@

streamUpMk l z = return (z,l,l)
{-# Inline [0] streamUpMk #-}

streamUpStep l h (z,i,j)
  | i > h     = return $ Done
  | j > h     = return $ Skip (z,i+1,l)
  | otherwise = return $ Yield (z:.(i:->j)) (z,i,j+1)
{-# Inline [0] streamUpStep #-}

streamDownMk h z = return (z,h,h)
{-# Inline [0] streamDownMk #-}

streamDownStep l h (z,i,j)
  | i < l     = return $ Done
  | j < l     = return $ Skip (z,i-1,h)
  | otherwise = return $ Yield (z:.(i:->j)) (z,i,j-1)
{-# Inline [0] streamDownStep #-}

instance (IndexStream (Z:.EdgeBoundary t)) => IndexStream (EdgeBoundary t)



instance Arbitrary (EdgeBoundary t) where
  arbitrary = do
    a <- choose (0,14) -- at most 15*15 nodes
    b <- choose (0,14)
    return $ a :-> b
  shrink (i:->j) = Prelude.fmap (\(k,l) -> k :-> l) $ shrink (i,j)



-- | TODO this is unbelievably slow right now

instance Monad m => Serial m (EdgeBoundary t) where
  series = do
    i <- TS.getNonNegative <$> series
    j <- TS.getNonNegative <$> series
    return $ i :-> j

