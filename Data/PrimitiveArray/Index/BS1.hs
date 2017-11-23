
module Data.PrimitiveArray.Index.BS1 where



-- ** @BS1@

-- | TODO The size calculations are off by a factor of two, exactly. Each
-- bitset (say) @00110@ has a mirror image @11001@, whose elements do not have
-- to be indexed. It has to be investigated if a version with exact memory
-- bounds is slower in indexing.
--
-- @linearIndex@ explicitly maps @BS1 0 whatever@ to @0@.

instance Index (BS1 i t) where
  newtype LimitType (BS1 i t) = LtBS1 (BitSet t)
--  -- TODO shouldn't this be @+1@ for the case where @s/=0@?
--  linearIndex (LtBS1 pc) (BS1 s i)
--    | s == 0    = 0
--    | otherwise = error "rewrite BS1 linearIndex" -- 1 + linearIndex (pc:.2^pc) (s:.i)
--  {-# INLINE linearIndex #-}
--  size (LtBS1 pc) = 2^pc * pc + 1
--  {-# INLINE size #-}
--  inBounds (LtBS1 pc) (BS1 s i) = popCount s <= pc && 0 <= i && getBoundary i <= pc
--  {-# INLINE inBounds #-}

instance IndexStream z => IndexStream (z:.BS1 i I) where
  streamUp   (ls:..l) (hs:..h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamUp   ls hs
--  streamDown (ls:.l) (hs:.h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamDown ls hs
  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}

--instance IndexStream z => IndexStream (z:.BS1 i O) where
--  streamUp   (ls:.l) (hs:.h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamUp   ls hs
--  streamDown (ls:.l) (hs:.h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}
--
--instance IndexStream z => IndexStream (z:.BS1 i C) where
--  streamUp   (ls:..l) (hs:..h) = flatten (streamUpBsIMk   l h) (streamUpBsIStep   l h) $ streamUp   ls hs
--  streamDown (ls:..l) (hs:..h) = flatten (streamDownBsIMk l h) (streamDownBsIStep l h) $ streamDown ls hs
--  {-# Inline streamUp #-}
--  {-# Inline streamDown #-}

instance IndexStream (Z:.BS1 i t) => IndexStream (BS1 i t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}


--streamUpBsIMk :: (Monad m) => BS1 a i -> BS1 b i -> z -> m (z, Maybe (BS1 c i))
streamUpBsIMk (LtBS1 sl) (LtBS1 sh) z = return (z, if sl <= sh then Just (BS1 sl (Boundary . max 0 $ lsbZ sl)) else Nothing)
{-# Inline [0] streamUpBsIMk #-}

--streamUpBsIStep :: (Monad m, SetPredSucc s) => s -> s -> (t, Maybe s) -> m (SM.Step (t, Maybe s) (t :. s))
streamUpBsIStep = undefined
--streamUpBsIStep l h (z , Nothing) = return $ SM.Done
--streamUpBsIStep l h (z,  Just t ) = return $ SM.Yield (z:.t) (z , setSucc l h t)
{-# Inline [0] streamUpBsIStep #-}

--streamDownBsIMk :: (Monad m) => BS1 a i -> BS1 b i -> z -> m (z, Maybe (BS1 c i))
--streamDownBsIMk (BS1 sl _) (BS1 sh _) z = return (z, if sl <= sh then Just (BS1 sh (Boundary . max 0 $ lsbZ sh)) else Nothing)
--{-# Inline [0] streamDownBsIMk #-}
--
--streamDownBsIStep :: (Monad m, SetPredSucc s) => s -> s -> (t, Maybe s) -> m (SM.Step (t, Maybe s) (t :. s))
--streamDownBsIStep l h (z , Nothing) = return $ SM.Done
--streamDownBsIStep l h (z , Just t ) = return $ SM.Yield (z:.t) (z , setPred l h t)
--{-# Inline [0] streamDownBsIStep #-}



