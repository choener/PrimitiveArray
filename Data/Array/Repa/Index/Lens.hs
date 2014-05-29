{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Lenses for index fields
--
-- TODO isos to and from tuples?
--
-- TODO should the guy @x@ in @(Z:.x:.y)@ be at @_1@ or should @y@ be at
-- @_1@?

module Data.Array.Repa.Index.Lens where

import           Control.Applicative
import           Control.Lens
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape



class IndexLens i where
  _from :: Functor k => (Int -> k Int) -> i -> k i
  _to   :: Functor k => (Int -> k Int) -> i -> k i



-- ** @Field@ instances for inductive tuples.

instance Field1 (Z:.a) (Z:.a') a a' where
  _1 k ~(Z:.a) = k a <&> \a' -> (Z:.a')
  {-# INLINE _1 #-}

instance Field1 (Z:.a:.b) (Z:.a':.b) a a' where
  _1 k ~(Z:.a:.b) = k a <&> \a' -> (Z:.a':.b)
  {-# INLINE _1 #-}

instance Field1 (Z:.a:.b:.c) (Z:.a':.b:.c) a a' where
  _1 k ~(Z:.a:.b:.c) = k a <&> \a' -> (Z:.a':.b:.c)
  {-# INLINE _1 #-}

instance Field1 (Z:.a:.b:.c:.d) (Z:.a':.b:.c:.d) a a' where
  _1 k ~(Z:.a:.b:.c:.d) = k a <&> \a' -> (Z:.a':.b:.c:.d)
  {-# INLINE _1 #-}

instance Field1 (Z:.a:.b:.c:.d:.e) (Z:.a':.b:.c:.d:.e) a a' where
  _1 k ~(Z:.a:.b:.c:.d:.e) = k a <&> \a' -> (Z:.a':.b:.c:.d:.e)
  {-# INLINE _1 #-}

instance Field1 (Z:.a:.b:.c:.d:.e:.f) (Z:.a':.b:.c:.d:.e:.f) a a' where
  _1 k ~(Z:.a:.b:.c:.d:.e:.f) = k a <&> \a' -> (Z:.a':.b:.c:.d:.e:.f)
  {-# INLINE _1 #-}

--

instance Field2 (Z:.a:.b) (Z:.a:.b') b b' where
  _2 k ~(Z:.a:.b) = k b <&> \b' -> (Z:.a:.b')
  {-# INLINE _2 #-}

instance Field2 (Z:.a:.b:.c) (Z:.a:.b':.c) b b' where
  _2 k ~(Z:.a:.b:.c) = k b <&> \b' -> (Z:.a:.b':.c)
  {-# INLINE _2 #-}

instance Field2 (Z:.a:.b:.c:.d) (Z:.a:.b':.c:.d) b b' where
  _2 k ~(Z:.a:.b:.c:.d) = k b <&> \b' -> (Z:.a:.b':.c:.d)
  {-# INLINE _2 #-}

instance Field2 (Z:.a:.b:.c:.d:.e) (Z:.a:.b':.c:.d:.e) b b' where
  _2 k ~(Z:.a:.b:.c:.d:.e) = k b <&> \b' -> (Z:.a:.b':.c:.d:.e)
  {-# INLINE _2 #-}

instance Field2 (Z:.a:.b:.c:.d:.e:.f) (Z:.a:.b':.c:.d:.e:.f) b b' where
  _2 k ~(Z:.a:.b:.c:.d:.e:.f) = k b <&> \b' -> (Z:.a:.b':.c:.d:.e:.f)
  {-# INLINE _2 #-}

--

instance Field3 (Z:.a:.b:.c) (Z:.a:.b:.c') c c' where
  _3 k ~(Z:.a:.b:.c) = k c <&> \c' -> (Z:.a:.b:.c')
  {-# INLINE _3 #-}

instance Field3 (Z:.a:.b:.c:.d) (Z:.a:.b:.c':.d) c c' where
  _3 k ~(Z:.a:.b:.c:.d) = k c <&> \c' -> (Z:.a:.b:.c':.d)
  {-# INLINE _3 #-}

instance Field3 (Z:.a:.b:.c:.d:.e) (Z:.a:.b:.c':.d:.e) c c' where
  _3 k ~(Z:.a:.b:.c:.d:.e) = k c <&> \c' -> (Z:.a:.b:.c':.d:.e)
  {-# INLINE _3 #-}

instance Field3 (Z:.a:.b:.c:.d:.e:.f) (Z:.a:.b:.c':.d:.e:.f) c c' where
  _3 k ~(Z:.a:.b:.c:.d:.e:.f) = k c <&> \c' -> (Z:.a:.b:.c':.d:.e:.f)
  {-# INLINE _3 #-}

--

instance Field4 (Z:.a:.b:.c:.d) (Z:.a:.b:.c:.d') d d' where
  _4 k ~(Z:.a:.b:.c:.d) = k d <&> \d' -> (Z:.a:.b:.c:.d')
  {-# INLINE _4 #-}

instance Field4 (Z:.a:.b:.c:.d:.e) (Z:.a:.b:.c:.d':.e) d d' where
  _4 k ~(Z:.a:.b:.c:.d:.e) = k d <&> \d' -> (Z:.a:.b:.c:.d':.e)
  {-# INLINE _4 #-}

instance Field4 (Z:.a:.b:.c:.d:.e:.f) (Z:.a:.b:.c:.d':.e:.f) d d' where
  _4 k ~(Z:.a:.b:.c:.d:.e:.f) = k d <&> \d' -> (Z:.a:.b:.c:.d':.e:.f)
  {-# INLINE _4 #-}

--

instance Field5 (Z:.a:.b:.c:.d:.e) (Z:.a:.b:.c:.d:.e') e e' where
  _5 k ~(Z:.a:.b:.c:.d:.e) = k e <&> \e' -> (Z:.a:.b:.c:.d:.e')
  {-# INLINE _5 #-}

instance Field5 (Z:.a:.b:.c:.d:.e:.f) (Z:.a:.b:.c:.d:.e':.f) e e' where
  _5 k ~(Z:.a:.b:.c:.d:.e:.f) = k e <&> \e' -> (Z:.a:.b:.c:.d:.e':.f)
  {-# INLINE _5 #-}

--

instance Field6 (Z:.a:.b:.c:.d:.e:.f) (Z:.a:.b:.c:.d:.e:.f') f f' where
  _6 k ~(Z:.a:.b:.c:.d:.e:.f) = k f <&> \f' -> (Z:.a:.b:.c:.d:.e:.f')
  {-# INLINE _6 #-}

