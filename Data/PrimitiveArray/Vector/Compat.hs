
module Data.PrimitiveArray.Vector.Compat
  ( flatten
  , Size(..)
  ) where

import qualified Data.Vector.Fusion.Stream.Monadic as SM

#if MIN_VERSION_vector(0,11,0)
import Data.Vector.Fusion.Bundle.Size
#else
import Data.Vector.Fusion.Stream.Size
#endif



flatten :: Monad m => (a -> m s) -> (s -> m (SM.Step s b)) -> SM.Stream m a -> SM.Stream m b
{-# Inline flatten #-}

#if MIN_VERSION_vector(0,11,0)
flatten = SM.flatten
#else
flatten = \mk step -> SM.flatten mk step Unknown
#endif

