
-- | This solution to holding a sparse set of elements for dynamic programming. The underlying
-- representation requires @O (log n)@ access time for each read or write, where @n@ is the number
-- of elements to be stored. It uses an experimental "bucketing" system to provide better lower and
-- upper bounds than otherwise possible.
--
-- TODO @ADPfusion / FillTyLvl@ handles actually filling the tables. In case all @BigOrder@ tables
-- are dense and of the same dimensional extent, we are fine. However if at least one table is
-- dense, while others are sparse, we will have write to nothing, which should not crash. In case of
-- all-sparse tables for a BigOrder, we have to calculate the union of all indices. This all is
-- currently not happening...
--
-- TODO require @readMaybe@ and @indexMaybe@ to return @Nothing@ on missing elements. This requires
-- an extension of the @Class@ structure for tables.

module Data.PrimitiveArray.SparseSearch where

import           Control.Monad.Primitive (PrimState,PrimMonad)
import           Control.Monad.ST (ST)
import           Debug.Trace (traceShow)
import qualified Control.Monad.State.Strict as SS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Search as VAS
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import           Data.PrimitiveArray.Class
import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index        -- TODO only while @SparseBucket@ is here



-- | @manhattan@ turns an index @sh@ into a starting point within 'sparseIndices' of the 'Sparse'
-- data structure. This should reduce the time required to search @sparseIndices@, because
-- @manhattanStart[manhattan sh]@ yields a left bound, while @manhattanStart[manhattan sh +1]@ will
-- yield an excluded right bound.
--
-- Uses the @Manhattan@ distance.
--
-- TODO This should probably be moved into the @Index@ module.

class SparseBucket sh where
  manhattan :: LimitType sh -> sh -> Int
  manhattanMax :: LimitType sh -> Int

instance SparseBucket Z where
  {-# Inline manhattan #-}
  manhattan ZZ Z = 0
  {-# Inline manhattanMax #-}
  manhattanMax ZZ = 1

-- | Manhattan distances add up.

instance (SparseBucket i, SparseBucket is) => SparseBucket (is:.i) where
  {-# Inline manhattan #-}
  manhattan (zz:..z) (is:.i) = manhattan zz is + manhattan z i
  {-# Inline manhattanMax #-}
  manhattanMax (zz:..z) = manhattanMax zz + manhattanMax z

instance SparseBucket (PointL I) where
  {-# Inline manhattan #-}
  manhattan (LtPointL u) (PointL i) = i
  {-# Inline manhattanMax #-}
  manhattanMax (LtPointL u) = u


-- |
--
-- TODO Is this instance correct? Outside indices shrink?

instance SparseBucket (PointL O) where
  {-# Inline manhattan #-}
  manhattan (LtPointL u) (PointL i) = u-i
  {-# Inline manhattanMax #-}
  manhattanMax (LtPointL u) = u

-- | This is a sparse matrix, where only a subset of indices have data associated.

data Sparse w v sh e = Sparse
  { sparseUpperBound  :: !(LimitType sh)
  -- ^ The upper bound for the DP matrix. Not the upper bound of indexes in use, but the theoretical
  -- upper bound.
  , sparseData        :: !(v e)
  -- ^ Vector with actually existing data.
  , sparseIndices     :: !(w sh)
  -- ^ The index of each @sh@ is the same as of the corresponding @sparseData@ structure. Indices
  -- should be ordered as required by the @streamUp@ function, to facilitate filling @Sparse@ by
  -- going from left to right.
  , manhattanStart    :: !(VU.Vector Int)
  -- ^ Provides left/right boundaries into @sparseIndices@ to speed up index search. Should be one
  -- larger than the largest index to look up, to always provides a good excluded bound.
  }

type Unboxed w sh e = Sparse w VU.Vector sh e
-- 
-- type Storable sh e = Dense VS.Vector sh e
-- 
-- type Boxed sh e = Dense V.Vector sh e



-- | Currently, our mutable variant of sparse matrices will keep indices and manhattan starts
-- immutable as well.

data instance MutArr m (Sparse w v sh e) = MSparse
  { msparseUpperBound :: !(LimitType sh)
  , msparseData       :: !(VG.Mutable v (PrimState m) e)
  , msparseIndices    :: !(w sh) -- (VG.Mutable w (PrimState m) sh)
  , mmanhattanStart   :: !(VU.Vector Int) -- (VU.MVector (PrimState m) Int)
  }
--  deriving (Generic,Typeable)


type family FillStruc arr :: *

type instance FillStruc (Sparse w v sh e) = (w sh)

class MPrimArrayVarOps (arr :: * -> * -> *) sh e where
  -- | Variant of 'newM' that requires a fill structure.
  vnewM :: (Monad m, PrimMonad m) => LimitType sh -> FillStruc (arr sh e) -> m (MutArr m (arr sh e))
  -- | Variant of 'newWithM'
  vnewWithM :: (Monad m, PrimMonad m) => LimitType sh -> FillStruc (arr sh e) -> e -> m (MutArr m (arr sh e))
  -- | Write into the mutable array, but if the index @sh@ does not exist, silently continue.
  vwriteM :: (Monad m, PrimMonad m) => MutArr m (arr sh e) -> sh -> e -> m ()
  -- | Read from the mutable array, but return @Nothing@ in case @sh@ does not exist. This will
  -- allow streaming DP combinators to "jump" over missing elements.
  --
  -- TODO we will have to check CORE to see if the @Maybe@ is successfully eliminated.
  vreadM :: (Monad m, PrimMonad m) => MutArr m (arr sh e) -> sh -> m (Maybe e)
  -- | Index into immutable array, testing to have only a single class
  vunsafeIndex :: arr sh e -> sh -> Maybe e

vnewWithPA
  ∷ (PrimMonad m, MPrimArrayVarOps arr sh elm, PrimArrayOps arr sh elm)
  ⇒ LimitType sh
  -> FillStruc (arr sh elm)
  → elm
  → m (arr sh elm)
vnewWithPA ub xs def = do
  ma ← vnewWithM ub xs def
  unsafeFreeze ma
{-# Inlinable vnewWithPA #-}



instance
  ( Index sh, SparseBucket sh, Eq sh, Ord sh
  , VG.Vector w sh
  , VG.Vector v e
  , Show sh, Show e
  ) => PrimArrayOps (Sparse w v) sh e where
  {-# Inline upperBound #-}
  upperBound Sparse{..} = sparseUpperBound
  {-# Inline unsafeFreeze #-}
  unsafeFreeze MSparse{..} = do
    let sparseUpperBound = msparseUpperBound
        sparseIndices = msparseIndices
        manhattanStart = mmanhattanStart
    sparseData <- VG.unsafeFreeze msparseData
    return Sparse{..}
  {-# Inline unsafeThaw #-}
  unsafeThaw Sparse{..} = do
    let msparseUpperBound = sparseUpperBound
        msparseIndices = sparseIndices
        mmanhattanStart = manhattanStart
    msparseData <- VG.unsafeThaw sparseData
    return MSparse{..}
  {-# Inline unsafeIndex #-}
  unsafeIndex Sparse{..} idx = case manhattanIndex sparseUpperBound manhattanStart sparseIndices idx of
      Nothing -> error "unsafeIndex of non-existing index"
      Just v  -> VG.unsafeIndex sparseData v


instance
  ( Index sh, Eq sh, Ord sh, SparseBucket sh
  , MutArr m (Sparse w v sh e) ~ mv
  , VGM.MVector (VG.Mutable v) e
  , VG.Vector w sh
  , MPrimArrayVarOps (Sparse w v) sh e
#if ADPFUSION_DEBUGOUTPUT
  , Show sh, Show (LimitType sh), Show e
#endif
  ) ⇒ MPrimArrayOps (Sparse w v) sh e where
  {-# Inline upperBoundM #-}
  upperBoundM MSparse{..} = msparseUpperBound
{-
  {-# Inline fromListM #-}
  fromListM h xs = do
    ma ← newM h
    -- TODO provide sparse storage
    let (MDense _ mba) = ma
    -- there need to be at least as many elements, as we want to fill. There could be more, in debug
    -- tests, we like to do @[0..]@ and this should not trigger the assert.
    SM.zipWithM_ (\k x → assert (length (Prelude.take (size h) xs) == size h) $ unsafeWrite mba k x) (SM.enumFromTo 0 (size h -1)) (SM.fromList xs)
    return ma
-}
  -- | @newM@ produces an essentially dense matrix.
  --
  -- TODO create a full set of indices
  {-# Inline newM #-}     -- TODO was NoInline, check if anything breaks!
  newM h = error "call of newM not supported for sparse matrices, use vnewM!" -- (\nul -> MSparse h nul VG.empty VG.empty) <$> VGM.new 0
{-
  {-# Inline newWithM #-}
  newWithM h def = do
    ma ← newM h
    let (MDense _ mba) = ma
    SM.mapM_ (\k → unsafeWrite mba k def) $ SM.enumFromTo 0 (size h -1)
    return ma
-}
  {-# Inline readM #-}
  readM MSparse{..} idx = do
    case manhattanIndex msparseUpperBound mmanhattanStart msparseIndices idx of
      Nothing -> error "read of non-existing element"
      Just v  -> VGM.unsafeRead msparseData v
  -- | Note that @writeM@ will fail loudly, because we can specialize in @FillTyLvl@ to use
  -- non-failing writes.
  {-# Inline writeM #-}
  writeM MSparse{..} idx elm = do
    case manhattanIndex msparseUpperBound mmanhattanStart msparseIndices idx of
      Nothing -> error "read of non-existing element"
      Just v  -> VGM.unsafeWrite msparseData v elm



instance
  ( VG.Vector w sh, SparseBucket sh, Ord sh
  , VG.Vector w (Int, sh)
  , VG.Vector w (Int, (Int, sh))
  , VGM.MVector (VG.Mutable v) e
  , VG.Vector v e
  ) => MPrimArrayVarOps (Sparse w v) sh e where
  {-# Inline vnewM #-}
  vnewM h fs' = do
    fs <- VG.thaw (VG.map (\i -> (manhattan h i, i)) fs') >>= \v -> VAI.sort v >> VG.unsafeFreeze v
    let msparseUpperBound = h
        msparseIndices = VG.force $ VG.map snd fs
        -- For any manhattan distance not found in the distances, we set to the length of the the
        -- @msparseIndices@ vector. Perform reverse-scan to update all manhattan start distances.
        go :: VU.MVector s Int -> ST s ()
        go mv = do
          VG.forM_ (VG.reverse $ VG.indexed fs) $ \(i,(mh,_)) -> VGM.write mv mh i
        mmanhattanStart = VG.modify go $ VG.replicate (manhattanMax h +1) (VG.length fs)
    msparseData <- VGM.new $ VG.length msparseIndices
    return $ MSparse {..}
  {-# Inline vnewWithM #-}
  vnewWithM h fs' e = do
    mv <- vnewM h fs'
    VGM.set (msparseData mv) e
    return mv
  {-# Inline vwriteM #-}
  vwriteM MSparse{..} sh e = case manhattanIndex msparseUpperBound mmanhattanStart msparseIndices sh of
      Nothing -> return ()
      Just v  -> VGM.unsafeWrite msparseData v e
  {-# Inline vreadM #-}
  vreadM MSparse{..} sh = case manhattanIndex msparseUpperBound mmanhattanStart msparseIndices sh of
      Nothing -> return Nothing
      Just v  -> Just <$> VGM.unsafeRead msparseData v
  {-# Inline vunsafeIndex #-}
  vunsafeIndex Sparse{..} = fmap (VG.unsafeIndex sparseData) . manhattanIndex sparseUpperBound manhattanStart sparseIndices



-- * Helper functions.

-- | Find the index with manhattan helper
--
-- TODO consider using binary search instead of a linear scan here!
-- e.g.: @k = VAS.binarySearchByBounds (==)@
--
-- NOTE running times with 100x100 DP problem "NeedlemanWunsch"
-- full findIndex of sixs:              0,050,000 cells/sec
-- using manhattan buckets, findIndex:  5,000,000 cells/sec
-- using binarySearch on slices:       11,000,000 cells/sec
--
-- On a 1000x1000 DP NeedlemanWunsch problem, binary search on slices is at 6,500,000 cells/sec.

manhattanIndex
  :: (SparseBucket sh, VG.Vector v sh, Eq sh, Ord sh)
  => LimitType sh -> Vector Int -> v sh -> sh -> Maybe Int
{-# Inline manhattanIndex #-}
--manhattanIndex ub mstart sixs idx = error "fix adding @l@" $ VG.findIndex (==idx) $ VG.unsafeSlice l (h-l+1) sixs
manhattanIndex ub mstart sixs idx = fmap (+l) . binarySearch idx $ VG.unsafeSlice l (h-l+1) sixs
  where
    b = manhattan ub idx
    -- lower and upper bucket bounds
    l = mstart `VU.unsafeIndex` b
    h = mstart `VU.unsafeIndex` (b+1)

binarySearch :: (Eq sh, Ord sh, VG.Vector v sh) => sh -> v sh -> Maybe Int
{-# Inline binarySearch #-}
binarySearch e v = go 0 (VG.length v -1)
  where
    go :: Int -> Int -> Maybe Int
    go !l !r =
      let !m = (r+l) `div` 2
          !x = VG.unsafeIndex v m
      in  if r<l then Nothing else case compare e x of
                                    LT -> go l (m-1)
                                    EQ -> Just m
                                    GT -> go (m+1) r


-- | Given two index vectors of the same shape, will return the correctly ordered vector of
-- the union of indices.
--
-- TODO This requires that @Ord (Shape O)@ uses the @Down@ instance of Ord! We need to fix this in
-- the @Index@ modules.
--
-- TODO Rewrite to allow fusion without intermediate vectors using uncons. This will make it
-- possible to chain applications. @stream@ should be fine for this.

mergeIndexVectors :: (Eq sh, Ord sh, VG.Vector w sh) => w sh -> w sh -> w sh
{-# Inlinable mergeIndexVectors #-}
mergeIndexVectors xs ys = VG.create $ do
  let lxs = VG.length xs
      lys = VG.length ys
  mv <- VGM.new $ lxs + lys
  let go !n !i !j
        | i>=lxs && j>=lys = return n
        | j>=lys = VG.unsafeIndexM xs i >>= VGM.unsafeWrite mv n >> go (n+1) (i+1) j
        | i>=lxs = VG.unsafeIndexM ys j >>= VGM.unsafeWrite mv n >> go (n+1) i (j+1)
        | otherwise = do
            x <- VG.unsafeIndexM xs i
            y <- VG.unsafeIndexM ys j
            if | x==y -> VGM.unsafeWrite mv n x >> go (n+1) (i+1) (j+1)
               | x< y -> VGM.unsafeWrite mv n x >> go (n+1) (i+1) j
               | x> y -> VGM.unsafeWrite mv n y >> go (n+1) i     (j+1)
  n <- go 0 0 0
  return $ VGM.unsafeTake n mv

