{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- | Operations to fill primitive arrays. Arrays are combined just like
-- indices using 'Z' and '(:.)'. This allows filling an unlimited number of
-- tables.

module Data.PrimitiveArray.FillTables where

import Control.Monad.Primitive
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Stream.Size
import Control.Monad (when)

import Data.PrimitiveArray.Class
import Data.Array.Repa.Index.Subword
import Data.Array.Repa.Index.Points
import Data.PrimitiveArray.Zero



-- * High-level table filling system.

-- | Run the forward phase of algorithms.

runFillTables (ts:.t) = S.mapM_ (unsafeWriteCell (ts:.t)) $ fillTables from to where
  (from,to) = boundsM t
{-# INLINE runFillTables #-}

-- | Captures creating the indices filling tables in the correct order. We
-- assume that it does *not* matter which of the tables is filled first. If
-- you need to fill tables in a sequential order, bind to s.th. like
-- @(Z:.s:.t)@ and then run @runFillTables (Z:.s) >> runFillTables (Z:.t)@.

class FillTables sh where
    fillTables :: Monad m => sh -> sh -> S.Stream m sh

instance FillTables Z where
    fillTables _ _ = S.singleton Z
    {-# INLINE fillTables #-}

-- | Create index stream with an active @PointL 0 m@. Note that we
-- currently assume that the first index is indeed 0.
--
-- TODO Rewrite to use the @HERMIT in the stream@ trick of stopping at 0.
-- Will require some changes to @step@. (We can't just do @[t,t-1,..f]@ as
-- we have the dependency that @k@ requires @k-1@.

instance FillTables is => FillTables (is:.PointL) where
    fillTables (fs:.PointL (0:.f)) (ts:.PointL (0:.t)) = S.flatten mk step Unknown $ fillTables fs ts where
      mk is = return (is:.f)
      step (is:.k)
        | k>t       = return $ S.Done
        | otherwise = return $ S.Yield (is:.pointL 0 k) (is:.(k+1))
      {-# INLINE [1] mk #-}
      {-# INLINE [1] step #-}
    {-# INLINE fillTables #-}



-- * Write to individuel cells.

-- | 'WriteCell' provides methods to fill all cells with a specific index
-- @sh@ in a stack of non-terminal tables @c@.

class WriteCell m c sh where
    unsafeWriteCell :: c -> sh -> m ()
    writeCell :: c -> sh -> m ()

instance (Monad m) => WriteCell m Z sh where
    unsafeWriteCell _ _ = return ()
    writeCell _ _ = return ()
    {-# INLINE unsafeWriteCell #-}
    {-# INLINE writeCell #-}

instance (f ~ (sh -> m a), t ~ MutArr m (arr sh a), WriteCell m cs sh, Monad m, MPrimArrayOps arr sh a, PrimMonad m) => WriteCell m (cs:.(t,f)) sh where
    unsafeWriteCell (cs:.(t,f)) sh = unsafeWriteCell cs sh >> f sh >>= writeM t sh
    writeCell (cs:.(t,f)) sh = writeCell cs sh >> when (inBoundsM t sh) (f sh >>= writeM t sh)
    {-# INLINE unsafeWriteCell #-}
    {-# INLINE writeCell #-}

