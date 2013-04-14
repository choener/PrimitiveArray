{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | Operations to fill primitive arrays. Arrays are combined just like indices
-- using 'Z' and '(:.)'. This allows filling an unlimited number of tables.
--
-- TODO make explicit in which order the tables are filled.

module Data.PrimitiveArray.FillTables where

import Control.Monad.Primitive
import Data.Array.Repa.Index
import Data.Array.Repa.Shape

import Data.PrimitiveArray
import Data.Array.Repa.Index.Subword



class UpperTriS stack where
  upperTriS :: stack -> m ()

instance UpperTriS (xs:.MutArr m (arr Subword e)) where

instance UpperTriS Z where


class Stack m sh xs fs where
  writeStack :: xs -> fs -> sh -> m ()

instance (Monad m) => Stack m sh Z Z where
  writeStack _ _ _ = return ()
  {-# INLINE writeStack #-}

instance
  ( PrimMonad m
  , Stack m Subword xs fs
  , MPrimArrayOps arr Subword e
  ) => Stack m Subword (xs:.MutArr m (arr Subword e)) (fs:.(Subword -> m e)) where
  writeStack (xs:.x) (fs:.f) i = writeStack xs fs i >> f i >>= writeM x i
  {-# INLINE writeStack #-}
