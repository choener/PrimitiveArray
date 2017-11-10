
module Data.PrimitiveArray.Index.Int where

import Data.Vector.Fusion.Stream.Monadic (map,Step(..))
import Prelude hiding (map)

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Vector.Compat



instance Index Int where
  type LimitType Int = Int
  linearIndex _ k = k
  {-# Inline linearIndex #-}
  size _ h = h+1
  {-# Inline size #-}
  inBounds h k = 0 <= k && k <= h
  {-# Inline inBounds #-}

instance IndexStream z => IndexStream (z:.Int) where
  streamUp (ls:.l) (hs:.h) = flatten mk step $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ Done
            | otherwise = return $ Yield (z:.k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.l) (hs:.h) = flatten mk step $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ Done
            | otherwise = return $ Yield (z:.k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream Int where
  streamUp l h = map (\(Z:.k) -> k) $ streamUp (Z:.l) (Z:.h)
  {-# Inline streamUp #-}
  streamDown l h = map (\(Z:.k) -> k) $ streamDown (Z:.l) (Z:.h)
  {-# Inline streamDown #-}

