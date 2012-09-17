{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- Several methods to fill tables. Most useful for unboxed tables as we can not
-- use lazyness. Multiple tables should be combined using the Repa-index style
-- with base case 'Z' and each table consisting of the table and a function to
-- fill the table.
--
-- Example Z:.(t,DIMk -> tType):.(s,DIMk -> sType)
-- where xType is the element type of the table x.

module Data.PrimitiveArray.FillTable where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape



-- | Fill tables starting from the diagonal, going into the (0,n) corner.
-- Meaning we fill the longest diagonal first, then shorter ones. If a stack of
-- tables is given, the table closest to the base case 'Z' is filled first,
-- meaning we fill left-to-right.
--
-- NOTE All elements in the diagonal should be independent. Especially if you
-- use fillDiagonalP.

class (Monad m) => FillDiagonal m t where
  -- | Fill a table serially, by diagonals, where each is filled serially. The
  -- order in which diagonals are filled is unspecified.
  fillDiagonalS :: t -> m ()
  -- | Fill a table serially, by diagonals, where each is filled in parallel.
  -- The order is again unspecified.
  fillDiagonalP :: t -> m ()

-- | Base case for triangular tables.

instance (Monad m) => FillDiagonal m Z where
  fillDiagonalS Z = return ()
  {-# INLINE fillDiagonalS #-}
  fillDiagonalP Z = return ()
  {-# INLINE fillDiagonalP #-}

-- | Fill a stack of triangular tables by first filling the tail, then the
-- head. The head is a table and a function to fill the table.

instance (Monad m, FillDiagonal m tail, Shape sh, FillDiagonal m (tbl,sh -> e)) => FillDiagonal m (tail:.(tbl,sh -> e)) where
  fillDiagonalS (tail:.(tbl,func)) = fillDiagonalS tail >> fillDiagonalS (tbl,func)
  {-# INLINE fillDiagonalS #-}
  fillDiagonalP (tail:.(tbl,func)) = fillDiagonalP tail >> fillDiagonalP (tbl,func)
  {-# INLINE fillDiagonalP #-}

