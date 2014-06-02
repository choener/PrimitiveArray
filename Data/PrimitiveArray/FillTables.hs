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
import Data.Vector.Fusion.Stream.Monadic as M
import Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Size
import Control.Monad (when)

import Data.PrimitiveArray.Class
import Data.Array.Repa.Index.Subword
import Data.Array.Repa.Index.Points
import Data.PrimitiveArray.Zero



-- * High-level table filling system.

-- | Run the forward phase of algorithms. Is *really* unsafe for now if
-- tables have different sizes, as in its broken.
--
-- TODO Need to run min/max on the bounds for all tables, not just the last
-- table. Otherwise we don't really need the distinction between save and
-- unsafe. This will have to be in @runFillTables@.

unsafeRunFillTables
  :: ( GenerateIndices a
     , WriteCell m (tail :. (MutArr m (arr a elm), t)) a
     , MPrimArrayOps arr a elm
     , Monad m
     , PrimMonad m
     )
  => (tail :. (MutArr m (arr a elm), t)) -> m ()

unsafeRunFillTables (ts:.(t,f)) = M.mapM_ (unsafeWriteCell (ts:.(t,f))) $ generateIndices from to where
  (from,to) = boundsM t -- TODO min/max over all tables
{-# INLINE unsafeRunFillTables #-}

-- | Captures creating the indices filling tables in the correct order. We
-- assume that it does *not* matter which of the tables is filled first. If
-- you need to fill tables in a sequential order, bind to s.th. like
-- @(Z:.s:.t)@ and then run @runFillTables (Z:.s) >> runFillTables (Z:.t)@.

class GenerateIndices sh where
    generateIndices :: Monad m => sh -> sh -> M.Stream m sh

instance GenerateIndices Z where
    generateIndices _ _ = M.singleton Z
    {-# INLINE generateIndices #-}

-- | Create index stream with an active @PointL 0 m@. Note that we
-- currently assume that the first index is indeed 0.
--
-- TODO Rewrite to use the @HERMIT in the stream@ trick of stopping at 0.
-- Will require some changes to @step@. (We can't just do @[t,t-1,..f]@ as
-- we have the dependency that @k@ requires @k-1@.

instance GenerateIndices is => GenerateIndices (is:.PointL) where
    generateIndices (fs:.PointL (0:.f)) (ts:.PointL (0:.t)) = M.flatten mk step Unknown $ generateIndices fs ts where
      mk is = return (is:.f)
      step (is:.k)
        | k>t       = return $ M.Done
        | otherwise = return $ M.Yield (is:.pointL 0 k) (is:.(k+1))
      {-# INLINE [1] mk #-}
      {-# INLINE [1] step #-}
    {-# INLINE generateIndices #-}

-- | An index stream for subwords.
--
-- TODO from/to for subwords??? (currently starting at (0:.0) always)

instance GenerateIndices is => GenerateIndices (is:.Subword) where
    generateIndices (fs:.Subword (0:.0)) (ts:.Subword (0:.t)) = M.flatten mk step Unknown $ generateIndices fs ts where
      mk is = return (is:.t:.t)
      step (is:.k:.l)
        | k<0       = return $ M.Done
        | l>t       = return $ M.Skip                    (is:.k-1:.k-1)
        | otherwise = return $ M.Yield (is:.subword k l) (is:.k  :.l+1)
      {-# INLINE [1] mk #-}
      {-# INLINE [1] step #-}
    {-# INLINE generateIndices #-}



-- * Write to individuel cells.

-- | 'WriteCell' provides methods to fill all cells with a specific index
-- @sh@ in a stack of non-terminal tables @c@.

class (Monad m) => WriteCell m c sh where
    unsafeWriteCell :: c -> sh -> m ()
    writeCell       :: c -> sh -> m ()

instance (Monad m) => WriteCell m Z sh where
    unsafeWriteCell _ _ = return ()
    writeCell _ _ = return ()
    {-# INLINE unsafeWriteCell #-}
    {-# INLINE writeCell #-}

instance (WriteCell m cs sh, Monad m, MPrimArrayOps arr sh a, PrimMonad m) => WriteCell m (cs:.(MutArr m (arr sh a), sh -> m a)) sh where
    unsafeWriteCell (cs:.(t,f)) sh = unsafeWriteCell cs sh >> (f sh >>= writeM t sh)
    writeCell (cs:.(t,f)) sh = writeCell cs sh >> (when (inBoundsM t sh) (f sh >>= writeM t sh))
    {-# INLINE unsafeWriteCell #-}
    {-# INLINE writeCell #-}

