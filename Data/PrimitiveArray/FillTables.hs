{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations to fill primitive arrays. Arrays are combined just like
-- indices using 'Z' and '(:.)'. This allows filling an unlimited number of
-- tables. 'ExtShape' provides the 'rangeStream' function with generates
-- a stream of indices in (generally) the right order.

module Data.PrimitiveArray.FillTables where

import Control.Monad.Primitive
import Control.Monad (when)
import Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic as M
import Data.Vector.Fusion.Stream.Size

import Data.PrimitiveArray.Class
import Data.PrimitiveArray.Index
import Data.PrimitiveArray.Unboxed



-- * High-level table filling system.

-- | Run the forward phase of algorithms. Is *really* unsafe for now if
-- tables have different sizes, as in its broken.
--
-- TODO Need to run min/max on the bounds for all tables, not just the last
-- table. Otherwise we don't really need the distinction between save and
-- unsafe. This will have to be in @runFillTables@.

unsafeRunFillTables
  :: ( Index sh, IndexStream sh
     , WriteCell m (tail :. (MutArr m (arr sh elm), t)) sh
     , MPrimArrayOps arr sh elm
     , Monad m
     , PrimMonad m
     )
  => (tail :. (MutArr m (arr sh elm), t)) -> m ()

unsafeRunFillTables (ts:.(t,f)) = M.mapM_ (unsafeWriteCell (ts:.(t,f))) $ streamUp from to where -- generateIndices from to where
  (from,to) = boundsM t -- TODO min/max over all tables [for the safe version, the unsafe version *always* assumes equal-size tables; we still should check this during runtime]
{-# INLINE unsafeRunFillTables #-}



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

