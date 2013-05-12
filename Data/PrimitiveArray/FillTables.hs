{-# LANGUAGE UndecidableInstances #-}
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
import Data.Vector.Fusion.Stream.Monadic as S

import Data.PrimitiveArray
import Data.Array.Repa.Index.Subword



-- * Driver classes for table filling system.

-- Upper triangular table filling. Right now, only a serial option 'upperTriS'
-- is available.
--
-- TODO Using Repa, 'upperTriP' will soon become available.

class UpperTriS m stack where
  upperTriS :: stack -> m ()

-- | Defines how a single index in a stack of arrays + evaluation functions is
-- handled. The instances *should* work for any index @ix@.

class Stack m sh xs where
  writeStack :: xs -> sh -> m ()



-- * Instances

-- ** 1-tape grammars with 'Subword' indices.

-- TODO Insert check that all extends are the same!

instance
  ( Monad m
  , MPrimArrayOps arr (Z:.Subword) e
  , Stack m Subword (xs :. SubwordNonTerminal m arr e)
  ) => UpperTriS m  (xs :. SubwordNonTerminal m arr e) where
  upperTriS xs@(_:.(x,f)) = do
    -- TODO missing extends check
    let (Z:.Subword (l:._),Z:.Subword (u:._)) = boundsM x
    S.mapM_ (go xs) $ unfolder l u
    where
      -- Write all table values at a certain subword. Note that tables are
      -- filled left to right in the stack order '(Z:.first:.next:.last)'
      go xs (i,j) = writeStack xs (Subword (i:.j))
      {-# INLINE go #-}
      -- the unfolder steps through the diagonals from the main toward the
      -- upper-right. It starts a the main-diagonal column and goes to the
      -- right in each big step '(i==u)' and increases the row by one in each
      -- small step.
      unfolder l u = S.unfoldr step (l,l) where
        step (j,i)
          | j> u      = Nothing
          | i==u      = Just ((i,j),(j+1,l))
          | otherwise = Just ((i,j),(j,i+1))
        {-# INLINE step #-}
      {-# INLINE unfolder #-}
  {-# INLINE upperTriS #-}

instance
  ( PrimMonad m
  , Stack m Subword xs
  , MPrimArrayOps arr (Z:.Subword) e
  ) => Stack m Subword (xs :. SubwordNonTerminal m arr e) where
  writeStack (xs:.(x,f)) i = writeStack xs i >> f i >>= writeM x (Z:.i)
  {-# INLINE writeStack #-}

-- ** Multi-tape indices.

instance (Monad m) => Stack m sh Z where
  writeStack _ _ = return ()
  {-# INLINE writeStack #-}

instance
  ( PrimMonad m
  , Stack m ix xs
  , MPrimArrayOps arr ix e
  ) => Stack m ix (xs :. GeneralNonTerminal m arr ix e) where
  writeStack (xs:.(x,f)) i = writeStack xs i >> f i >>= writeM x i
  {-# INLINE writeStack #-}



-- Wrap non-terminal symbol type, corresponding rule type.

type SubwordNonTerminal m arr e = (MutArr m (arr (Z:.Subword) e), Subword -> m e)
type GeneralNonTerminal m arr ix e = (MutArr m (arr ix e), ix -> m e)

