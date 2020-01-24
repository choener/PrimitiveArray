
{-# Language MagicHash #-}

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
-- This version requires working @fromLinearIndex@ but is potentially faster.

module Data.PrimitiveArray.Sparse.IntBinSearch where

import           Control.Monad.Primitive (PrimState,PrimMonad)
import           Control.Monad.ST (ST)
import           Data.Bits.Extras (msb)
import           Debug.Trace (traceShow)
import qualified Control.Monad.State.Strict as SS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector.Algorithms.Radix as Sort
import qualified Data.Vector.Algorithms.Search as VAS
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           GHC.Exts ( Int(..), Int#(..), (==#), (-#), (/=#), (*#), (+#), (<=#), remInt#, quotInt#, uncheckedIShiftRA#, (<#) )

import           Data.PrimitiveArray.Class
import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index        -- TODO only while @SparseBucket@ is here



-- | This is a sparse matrix, where only a subset of indices have data associated.

data Sparse w v sh e = Sparse
  { sparseUpperBound  :: !(LimitType sh)
  -- ^ The upper bound for the DP matrix. Not the upper bound of indexes in use, but the theoretical
  -- upper bound.
  , sparseData        :: !(v e)
  -- ^ Vector with actually existing data.
  , sparseIndices     :: !(VU.Vector Int)
  -- ^ Linearly encoded sparse indices
  , manhattanStart    :: !(VU.Vector Int)
  -- ^ Provides left/right boundaries into @sparseIndices@ to speed up index search. Should be one
  -- larger than the largest index to look up, to always provides a good excluded bound.
  }

type Unboxed  w sh e = Sparse w VU.Vector sh e

type Storable w sh e = Sparse w VS.Vector sh e

type Boxed    w sh e = Sparse w  V.Vector sh e



-- | Currently, our mutable variant of sparse matrices will keep indices and manhattan starts
-- immutable as well.

data instance MutArr m (Sparse w v sh e) = MSparse
  { msparseUpperBound :: !(LimitType sh)
  , msparseData       :: !(VG.Mutable v (PrimState m) e)
  , msparseIndices    :: !(VU.Vector Int)
  , mmanhattanStart   :: !(VU.Vector Int)
  }
--  deriving (Generic,Typeable)


type instance FillStruc (Sparse w v sh e) = (w sh)



instance
  ( Index sh, SparseBucket sh, Eq sh, Ord sh
  , VG.Vector w sh , VG.Vector w (Int,sh), VG.Vector w (Int,(Int,sh)), VG.Vector w (Int,Int), VG.Vector w Int
  , VG.Vector v e
#if ADPFUSION_DEBUGOUTPUT
  , Show sh, Show (LimitType sh), Show e
#endif
  ) => PrimArrayOps (Sparse w v) sh e where

  -- ** pure operations

  {-# Inline upperBound #-}
  upperBound Sparse{..} = sparseUpperBound
  {-# Inline unsafeIndex #-}
  unsafeIndex Sparse{..} idx = case manhattanIndex sparseUpperBound manhattanStart sparseIndices idx of
      Nothing -> error "unsafeIndex of non-existing index"
      Just v  -> VG.unsafeIndex sparseData v
  {-# Inline safeIndex #-}
  safeIndex Sparse{..} = fmap (VG.unsafeIndex sparseData) . manhattanIndex sparseUpperBound manhattanStart sparseIndices

  -- ** monadic operations

  {-# Inline unsafeFreezeM #-}
  unsafeFreezeM MSparse{..} = do
    let sparseUpperBound = msparseUpperBound
        sparseIndices = msparseIndices
        manhattanStart = mmanhattanStart
    sparseData <- VG.unsafeFreeze msparseData
    return Sparse{..}
  {-# Inline unsafeThawM #-}
  unsafeThawM Sparse{..} = do
    let msparseUpperBound = sparseUpperBound
        msparseIndices = sparseIndices
        mmanhattanStart = manhattanStart
    msparseData <- VG.unsafeThaw sparseData
    return MSparse{..}
  {-# Inline upperBoundM #-}
  upperBoundM MSparse{..} = msparseUpperBound
  {-# Inline newM #-}
  newM = error "not implemented, use newSM"
  {-# Inline newWithM #-}
  newWithM = error "not implemented, use newWithSM"
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
  {-# Inline newSM #-}
  newSM h fs' = do
    let msparseUpperBound = h
        -- sort sparse indices by (manhattan, linearIndex)
        {-# Inline srt #-}
        srt x y = let ix = fromLinearIndex h x
                      iy = fromLinearIndex h y
                  in  compare (manhattan h ix, x) (manhattan h iy, y)
        {-# Inline radixsrt #-}
        radixsrt i x = let ix = fromLinearIndex h x in Sort.radix i (manhattan h ix, x)
    msparseIndices <- do
      marr <- VG.thaw (VU.convert $ VG.map (linearIndex h) fs')
      Sort.sortBy (Sort.passes (undefined :: (Int,Int))) (Sort.size (undefined :: Int)) radixsrt marr
      VG.unsafeFreeze marr
    let -- For any manhattan distance not found in the distances, we set to the length of the the
        -- @msparseIndices@ vector. Perform reverse-scan to update all manhattan start distances.
        go :: VU.MVector s Int -> ST s ()
        {-# Inline go #-}
        go mv = do
          VG.forM_ (VG.reverse $ VG.indexed msparseIndices) $ \(i,k) -> let lix = fromLinearIndex h k; mh = manhattan h lix in VGM.write mv mh i
    let mmanhattanStart = VU.modify go $ VG.replicate (manhattanMax h +1) (VG.length msparseIndices)
    msparseData <- VGM.new $ VG.length msparseIndices
    return $ MSparse {..}
  {-# Inline newWithSM #-}
  newWithSM h fs' e = do
    mv <- newSM h fs'
    VGM.set (msparseData mv) e
    return mv
  {-# Inline safeWriteM #-}
  safeWriteM MSparse{..} sh e = case manhattanIndex msparseUpperBound mmanhattanStart msparseIndices sh of
      Nothing -> return ()
      Just v  -> VGM.unsafeWrite msparseData v e
  {-# Inline safeReadM #-}
  safeReadM MSparse{..} sh = case manhattanIndex msparseUpperBound mmanhattanStart msparseIndices sh of
      Nothing -> return Nothing
      Just v  -> Just <$> VGM.unsafeRead msparseData v



instance (Index sh, VG.Vector v e, VG.Vector v e') ⇒ PrimArrayMap (Sparse w v) sh e e' where
  {-# Inline mapArray #-}
  mapArray f sparse = sparse{sparseData = VG.map f (sparseData sparse)}





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
  :: (SparseBucket sh, Index sh)
  => LimitType sh -> Vector Int -> VU.Vector Int -> sh -> Maybe Int
{-# Inline manhattanIndex #-}
manhattanIndex ub mstart sixs idx = fmap (+l) . binarySearch (linearIndex ub idx) $ VG.unsafeSlice l (h-l) sixs
  where
    b = manhattan ub idx
    -- lower and upper bucket bounds
    l = mstart `VU.unsafeIndex` b
    h = mstart `VU.unsafeIndex` (b+1)

binarySearch :: Int -> VU.Vector Int -> Maybe Int
{-# Inline binarySearch #-}
{-
binarySearch k xs =
  let r1 = binarySearch1 k xs
      r2 = binarySearch2 k xs
  in if r1==r2 then r1 else error $ show (k,xs,r1,r2)
-}
{-
-- 1000x1000 at @1000 yields 3,050,000 cells / second
binarySearch (I# e) v = go 0 pp
  where
    -- largest index to check
    (I# r) = VG.length v -1
    -- largest power of two <= (r+1)
    pp = (2 ^ (max 0 $ msb $ VG.length v -1))
    -- wrap the actual non-branching worker function
    go :: Int -> Int -> Maybe Int
    {-# Inline go #-}
    go (I# l) (I# p) = let i = I# (go' l p) in if (VG.length v<1 || i<0) then Nothing else Just i
    -- @go'@ should be non-branching, and use a minimal number of array reads.
    go' :: Int# -> Int# -> Int#
    {-# Inline go' #-}
    go' l p
      -- we are done and will return the proposed position of the last element found or -1
      | 1# <- p ==# 0# = (e ==# x) *# l -# (e /=# x)
      | otherwise      = let i = go' (l +# (p *# chk *# leq)) (quotInt# p 2#) -- (uncheckedIShiftRA# p 1#)
                             -- (I# ii) = traceShow (I# l, I# p, I# i2r, I# x, I# leq) (I# i)
                         in  i -- i
      where i2r    = l +# (p *# chk) -- index to read
            (I# x) = VU.unsafeIndex v (I# i2r)
            leq    = x <=# e
            newl   = l +# p
            chk    = newl <=# r
-}
--
-- 1000x1000 at @1000 yields 6,030,000 cells / second
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
--


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

