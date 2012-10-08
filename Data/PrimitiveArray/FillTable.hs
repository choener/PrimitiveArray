{-# LANGUAGE TypeFamilies #-}
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
import Data.PrimitiveArray
import Control.Monad
import Control.Monad.Primitive (PrimState (..))



-- | Fill tables starting from the diagonal, going into the (0,n) corner.
-- Meaning we fill the longest diagonal first, then shorter ones. If a stack of
-- tables is given, the table closest to the base case 'Z' is filled first,
-- meaning we fill left-to-right.
--
-- NOTE All elements in the diagonal should be independent. Especially if you
-- use fillDiagonalP.
--
-- TODO fillDiagonalP stuff should take an 'Int' as last parameter. This param
-- goes "[0 .. n-1]" where "n" is the number of threads using Repa gangs. Each
-- action that fills many diagonal elements figures out which elements to fill
-- using this parameter in a modulo-style way.

class (Monad m) => FillDiagonal m t where
  -- | Fill a table serially, by diagonals, where each is filled serially. The
  -- order in which diagonals are filled is unspecified.
  fillDiagonalS :: t -> m ()
  -- | Fill a table serially, by diagonals, where each is filled in parallel.
  -- The order is again unspecified.
  fillDiagonalP :: t -> m ()

instance (Monad m, PrimMonad m, s ~ PrimState m, MPrimArrayOps tbl DIM2 e) => FillDiagonal m (tbl s DIM2 e, DIM2 -> e) where
  fillDiagonalS (t,f) = do let (lb,Z:.r:.c) = boundsM t
                           forM_ [0..c] $ \d -> do
                           forM_ [0..r-d] $ \k -> do
                             let i = d+k
                             let j = k
                             writeM t (Z:.i:.j) (f $ Z:.i:.j)
  {-# INLINE fillDiagonalS #-}



-- | Fill a table 't' using rule 'r'. Allows us to have one type class which
-- changes behaviour based on rules. 'fillTableS' and 'fillTableP' fill the
-- table serially or in parallel, depending on the rule chosen.
--
-- TODO and yes, 'fillTableP' is missing until we connect to repa3.
--
-- TODO preferable, we'd like to be polymorphic over dimensions.

class (Monad m) => FillTable m r t where
  fillTableS :: r -> t -> m ()

