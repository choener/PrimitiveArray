
module Data.PrimitiveArray.Index.Int where

import qualified Data.Vector.Fusion.Stream.Monadic as SM

import           Data.PrimitiveArray.Index.Class



instance Index Int where
  newtype LimitType Int = LtInt Int
  linearIndex _ k = k
  {-# Inline linearIndex #-}
  size (LtInt h) = h+1
  {-# Inline size #-}
  inBounds (LtInt h) k = 0 <= k && k <= h
  {-# Inline inBounds #-}
  zeroBound = 0
  {-# Inline [0] zeroBound #-}
  zeroBound' = LtInt 0
  {-# Inline [0] zeroBound' #-}
  totalSize (LtInt h) = [fromIntegral $ h+1]
  {-# Inline [0] totalSize #-}

deriving instance Show (LimitType Int)

instance IndexStream z => IndexStream (z:.Int) where
  streamUp (ls:.. LtInt l) (hs:.. LtInt h) = SM.flatten mk step $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:..LtInt l) (hs:..LtInt h) = SM.flatten mk step $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream Int where
  streamUp l h = SM.map (\(Z:.k) -> k) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# Inline streamUp #-}
  streamDown l h = SM.map (\(Z:.k) -> k) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# Inline streamDown #-}

