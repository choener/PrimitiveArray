
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Index structures for unordered data types. We use this for Hamiltonian
-- path problems, where we need sets with an interface.
--
-- Note that sets with interfaces have no topmost index. While bare sets
-- have the ``full set'', interfaces establish a partial order.
--
-- TODO for sets with an interface, should we keep the interfaced element
-- in the set or not? Currently, the interface elements are not in the set
-- itself.
--
-- TODO from this code it looks like the argument type of rangestream could
-- actually be a type family; since we don't always grab all elements.
-- Here, interfaces are defined by the bitset, so they are not important
-- for the rangestream initialization.
--
-- TODO instead of addDim/subDim we should have something along the lines
-- of @requiredCells@ for purposes of storage.

module Data.Array.Repa.Index.Set where

import           Data.Aeson
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.Binary
import           Data.Bits
import qualified Data.Bits.Extras as B
import           Data.Bits.Extras (Ranked)
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as VU

import           Data.Array.Repa.ExtShape
import           Data.Array.Repa.Index.Outside
import           Data.Bits.Ordered



-- | Certain sets have an interface, a particular element with special
-- meaning. In this module, certain ``meanings'' are already provided.
-- These include a @First@ element and a @Last@ element. We phantom-type
-- these to reduce programming overhead.

newtype Interface t = Interface Int
  deriving (Eq,Ord,Read,Show,Generic)

data First

data Last

-- | Newtype for a bitset. We'd use @Word@s but that requires more shape
-- instances.

newtype BitSet = BitSet Int
  deriving (Eq,Ord,Read,Show,Generic,FiniteBits,Ranked,Num,Bits)

instance Binary    BitSet
instance Serialize BitSet
instance ToJSON    BitSet
instance FromJSON  BitSet

derivingUnbox "BitSet"
  [t| BitSet     -> Int |]
  [| \(BitSet s) -> s   |]
  [| BitSet             |]

-- | newtype for wrapping complex set structures. The assumption is that
-- the @i@ in @IndexSet@ is again a shape structure.
--
-- TODO needs to be written, but postponed until the rest of the machinery
-- works. And is needed only for multi-tape set systems ... which have
-- interesting running times.

newtype IndexSet i = IndexSet i



-- TODO addDim should make it possible to have both, the full @l@ and the
-- full @r@ set. Right now, this is mostly a hack that assumes that @r@ is
-- full at the time of addition.
--
-- TODO This whole thing is a bit ``ad-hoc'' with sets, and all that.

instance Shape BitSet where
  {-# INLINE [1] rank #-}
  rank _ = 1
  {-# INLINE [1] zeroDim #-}
  zeroDim = BitSet 0
  {-# INLINE [1] unitDim #-}
  unitDim = BitSet 1
  {-# INLINE [1] intersectDim #-}
  intersectDim = error "sh:.PathSet / intersectDim"
  {-# INLINE [1] addDim #-}
  addDim (BitSet l) (BitSet r)
    = BitSet (l `shiftL` popCount r)
  {-# INLINE [1] size #-}
  size (BitSet s)
    = size (Z:.s)
  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (BitSet _) = True
  {-# INLINE [1] toIndex #-}
  toIndex (BitSet sS) (BitSet s)
    = toIndex (Z:.sS) (Z:.s)
  {-# INLINE [1] fromIndex #-}
  fromIndex = error "sh:.PathSet / fromIndex"
  {-# INLINE [1] inShapeRange #-}
  inShapeRange = error "sh:.PathSet / inShapeRange"
  {-# NOINLINE listOfShape #-}
  listOfShape = error "sh:.PathSet / listOfShape"
  {-# NOINLINE shapeOfList #-}
  shapeOfList = error "sh:.PathSet / shapeOfList"
  {-# INLINE deepSeq #-}
  deepSeq p x = p `seq` x

instance ExtShape BitSet where
  {-# INLINE [1] subDim #-}
  subDim (BitSet l) (BitSet r)
    = BitSet (l `shiftR` popCount r)
  rangeList = error "not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (BitSet 0) (BitSet r) =
    let v = popCntMemoInt (popCount r)
        go k = Just (BitSet $ VU.unsafeIndex v k, k+1)
    in  M.unfoldrN (VU.length v) go 0
  {-# INLINE topmostIndex #-}
  topmostIndex (BitSet f) (BitSet t) = BitSet t

instance ExtShape (Outside BitSet) where
  {-# INLINE [1] subDim #-}
  subDim (O l) (O r)
    = O $ subDim l r
  rangeList = error "not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (O (BitSet 0)) (O (BitSet r)) =
    let v = popCntMemoInt (popCount r)
        go k = Just (O . BitSet $ VU.unsafeIndex v k, k-1)
    in  M.unfoldrN (VU.length v) go (VU.length v -1)
  {-# INLINE topmostIndex #-}
  topmostIndex (O f) (O t) = O f




-- | All interfaces are isomorphic on the shape level.

instance Shape z => Shape (z:.Interface i) where
  {-# INLINE [1] rank #-}
  rank (sh:._) = rank sh + 1
  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. Interface 0
  {-# INLINE [1] unitDim #-}
  -- TODO do we need 1/1/1 or 1/0/0 ? pretty sure about 1 1 1 but I am
  -- using popcounts in size/toIndex currently anyway (which I shouldn't!)
  -- but at least the "pointed to element" stays the same as it is shifted
  -- as well (which probably means I should use @rs `shiftR` popCount ls
  -- + ls@
  unitDim = unitDim :. Interface 1
  {-# INLINE [1] intersectDim #-}
  intersectDim = error "sh:.PathSet / intersectDim"
  {-# INLINE [1] addDim #-}
  addDim (sh1 :. Interface l) (sh2 :. Interface r)
    = addDim sh1 sh2 :. Interface (l+r)
  {-# INLINE [1] size #-}
  size (sh1 :. Interface i)
    = size sh1 * i
  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. Interface i)
    | size sh1 > 0 = i < maxBound `div` size sh1
    | otherwise    = False
  {-# INLINE [1] toIndex #-}
  -- Recast the calculation in terms known to repa
  -- TODO check this!
  toIndex (shL :. Interface l) (shR :. Interface r)
    = toIndex shL shR * l + r
  {-# INLINE [1] fromIndex #-}
  fromIndex = error "sh:.PathSet / fromIndex"
  {-# INLINE [1] inShapeRange #-}
  inShapeRange = error "sh:.PathSet / inShapeRange"
  {-# NOINLINE listOfShape #-}
  listOfShape = error "sh:.PathSet / listOfShape"
  {-# NOINLINE shapeOfList #-}
  shapeOfList = error "sh:.PathSet / shapeOfList"
  {-# INLINE deepSeq #-}
  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

-- ExtShapes need to be more explicit to be able to capture the set
-- information

instance ExtShape (BitSet:.Interface i) where
  {-# INLINE [1] subDim #-}
  subDim (shL:.Interface l) (shR:.Interface r)
    = subDim shL shR :. Interface (l-r)
  rangeList = error "not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (shL:.Interface l) (shR:.Interface r) = M.flatten mk step Unknown $ rangeStream shL shR
    where mk s = return (s,lsbActive s)
          step (s,a)
            | a<0       = return $ M.Done
            | otherwise = return $ M.Yield (clearBit s a :. Interface a) (s,nextActive a s)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  topmostIndex = error "not implemented" -- there is none, sets with interfaces are partially ordered.

instance ExtShape (Outside (BitSet:.Interface i)) where
  {-# INLINE [1] subDim #-}
  subDim (O (shL:.Interface l)) (O (shR:.Interface r))
    = O $ subDim shL shR :. Interface (l-r)
  rangeList = error "not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (O (shL:.Interface l)) (O (shR:.Interface r)) = M.flatten mk step Unknown $ rangeStream (O shL) (O  shR)
    where mk (O s) = return (s,lsbActive s)
          step (s,a)
            | a<0       = return $ M.Done
            | otherwise = return $ M.Yield (O $ clearBit s a :. Interface a) (s,nextActive a s)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  topmostIndex = error "not implemented" -- there is none, sets with interfaces are partially ordered.

instance ExtShape (BitSet:. Interface i:.Interface j) where
  {-# INLINE [1] subDim #-}
  subDim (shL:.Interface l1:.Interface l2) (shR:.Interface r1:.Interface r2)
    = subDim shL shR :. Interface (l1-r1) :. Interface (l2-r2)
  rangeList = error "not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (shL:.Interface _:.Interface _) (shR:.Interface _:.Interface _)
    = M.flatten mk step Unknown $ rangeStream shL shR
    where mk s = let a = lsbActive s in return (s,a,nextActive a s)
          step (s,a,b)
            | a<0        = return $ M.Done
            | b<0        = do let a' = nextActive a s
                              return $ M.Skip (s,a',nextActive a' s)
            | otherwise  = do let s' = clearBit (clearBit s a) b
                              return $ M.Yield (s' :. Interface a :. Interface b) (s,a,nextActive b s)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  topmostIndex = error "not implemented"

instance ExtShape (Outside (BitSet:. Interface i:.Interface j)) where
  {-# INLINE [1] subDim #-}
  subDim (O (shL:.Interface l1:.Interface l2)) (O (shR:.Interface r1:.Interface r2))
    = O $ subDim shL shR :. Interface (l1-r1) :. Interface (l2-r2)
  rangeList = error "not implemented"
  {-# INLINE rangeStream #-}
  rangeStream (O (shL:.Interface _:.Interface _)) (O (shR:.Interface _:.Interface _))
    = M.flatten mk step Unknown $ rangeStream (O shL) (O shR)
    where mk (O s) = let a = lsbActive s in return (s,a,nextActive a s)
          step (s,a,b)
            | a<0        = return $ M.Done
            | b<0        = do let a' = nextActive a s
                              return $ M.Skip (s,a',nextActive a' s)
            | otherwise  = do let s' = clearBit (clearBit s a) b
                              return $ M.Yield (O $ s' :. Interface a :. Interface b) (s,a,nextActive b s)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  topmostIndex = error "not implemented"



{-

-- | a path set denotes a set of visited nodes 'psSet' together with the
-- node that was visited first 'psFirst' and the node visited last
-- 'psLast'.
--
-- NOTE we use 'Int' here, as @Int@s have good optimization and many
-- operations assume @Int@s instead of @Word@s.
--
-- TODO should @psSet@ contain @psFirst@ and @psLast@ or not? Currently:
-- yes! (we want to differentiate having visited nothing (all @PathSet
-- 0 0 0@) to having visited just node 0 @(PathSet 1 0 0)@. Have @psFirst@
-- and @psLast@ in @psSet@ increases the memory requirements to @N*N*2^N@
-- from @N*N*2^(N-2)@ i.e. by a factor of 4.
--
-- TODO newtype PathSet = PathSet { Z:.Int:.Int:.Int }
--
-- TODO rangeStream currently uses explicitly constructed vectors for the
-- sets. It would be better to be able to enumerate these explicitly. This
-- will come with a later version of the OrderedBits library.

data PathSet = PathSet
  { psSet   :: {-# UNPACK #-} !Int
  , psFirst :: {-# UNPACK #-} !Int
  , psLast  :: {-# UNPACK #-} !Int
  }
  deriving (Eq,Ord,Show,Generic)

derivingUnbox "PathSet"
  [t| PathSet -> (Int,Int,Int) |]
  [| \ (PathSet s f l) -> (s,f,l) |]
  [| \ (s,f,l) -> PathSet s f l |]

instance Binary    PathSet
instance Serialize PathSet
instance ToJSON    PathSet
instance FromJSON  PathSet



instance Shape sh => Shape (sh:.PathSet) where
  {-# INLINE [1] rank #-}
  rank (sh:._) = rank sh + 1
  {-# INLINE [1] zeroDim #-}
  zeroDim = zeroDim :. PathSet 0 0 0
  {-# INLINE [1] unitDim #-}
  -- TODO do we need 1/1/1 or 1/0/0 ? pretty sure about 1 1 1 but I am
  -- using popcounts in size/toIndex currently anyway (which I shouldn't!)
  -- but at least the "pointed to element" stays the same as it is shifted
  -- as well (which probably means I should use @rs `shiftR` popCount ls
  -- + ls@
  unitDim = unitDim :. PathSet 1 1 1
  {-# INLINE [1] intersectDim #-}
  intersectDim = error "sh:.PathSet / intersectDim"
  {-# INLINE [1] addDim #-}
  addDim (sh1 :. PathSet ls lf ll) (sh2 :. PathSet rs rf rl)
    = addDim sh1 sh2 :. PathSet (ls `shiftL` popCount rs) (lf+rf) (ll+rl)
  {-# INLINE [1] size #-}
  size (sh1 :. PathSet s _ _)
    = let !p = popCount s in size (sh1:.s:.p:.p)
    -- let p = popCount s + 1 in size sh1 * s * p * p
  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (sh1 :. PathSet p _ _)
    | size sh1 > 0 = p < maxBound `div` size sh1
    | otherwise    = False
  {-# INLINE [1] toIndex #-}
  -- Recart the calculation in terms known to repa
  -- TODO check this!
  toIndex (shF :. PathSet sS fF lL) (sh :. PathSet s f l)
    = let !p = popCount sS
      in  toIndex (shF:.sS:.p:.p) (sh:.s:.f:.l)
  {-
    = let p = popCount sS + 1
      in  toIndex shF sh * (sS * p * p)
          + s * p * p + f * p + l
  -}
  {-# INLINE [1] fromIndex #-}
  fromIndex = error "sh:.PathSet / fromIndex"
  {-# INLINE [1] inShapeRange #-}
  inShapeRange = error "sh:.PathSet / inShapeRange"
  {-# NOINLINE listOfShape #-}
  listOfShape = error "sh:.PathSet / listOfShape"
  {-# NOINLINE shapeOfList #-}
  shapeOfList = error "sh:.PathSet / shapeOfList"
  {-# INLINE deepSeq #-}
  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

instance ExtShape sh => ExtShape (sh:.PathSet) where
  {-# INLINE [1] subDim #-}
  subDim (sh1 :. PathSet ls lf ll) (sh2 :. PathSet rs rf rl)
    = subDim sh1 sh2 :. PathSet (ls `shiftR` popCount rs) (lf-rf) (ll-rl)
  {-# INLINE rangeList #-}
  rangeList _ _ = error "rangeList/not implemented"
  {-# INLINE rangeStream #-}
  -- we assume a set starting at "nothing set"
  rangeStream (sh1 :. PathSet 0 0 0) (sh2 :. PathSet rs rf rl) = M.flatten mk step Unknown $ rangeStream sh1 sh2
    where mk is = let v = popCntMemoInt (popCount rs)
                  in  return $ Left (is,v)
          step (Left (!is,!v))
            | G.null v  = return $ M.Done
            | pcnt == 0 = return $ M.Yield (is:.PathSet  0 0 0) $ Left  (is,vt) -- only one case
            | pcnt == 1 = return $ M.Yield (is:.PathSet vh l l) $ Left  (is,vt) -- only one case
            | otherwise = return $ M.Skip                       $ Right (is,v,vh,vh `clearBit` l)  -- prepare bit pools
            where pcnt = popCount  vh
                  l    = lsbActive vh
                  vh   = G.unsafeHead v
                  vt   = G.unsafeTail v
          step (Right (is,v,fp,lp))
            | fp==0     = return $ M.Skip                         $ Left  (is,vt)  -- the fp pool is empty, reset everything
            | lp==0     = return $ M.Skip                         $ Right (is,v,fn,vh `clearBit` lsbActive fn) -- new lp pool with cutten fp
            | otherwise = return $ M.Yield (is:.PathSet vh af al) $ Right (is,v,fp,lp `clearBit` al) -- just continue with next lp pool element
            where vh = G.unsafeHead v
                  vt = G.unsafeTail v
                  af = lsbActive fp
                  al = lsbActive lp
                  fn = fp `clearBit` af
          {-# INLINE [1] mk #-}
          {-# INLINE [1] step #-}
  {-# INLINE topmostIndex #-}
  topmostIndex _ _ = error "topmostIndex/not implemented"

instance (ExtShape sh) => ExtShape (sh :. Outside PathSet) where
  {-# INLINE [1] subDim #-}
  subDim (sh1 :. O z1) (sh2 :. O z2)
    = let (sh :. z) = subDim (sh1 :. z1) (sh2 :. z2) in sh :. O z
  {-# INLINE rangeList #-}
  rangeList _ _ = error "rangeList / Outside"
  {-# INLINE rangeStream #-}
  rangeStream (sh1 :. O (PathSet 0 0 0)) (sh2 :. O (PathSet rs rf rl)) = M.flatten mk step Unknown $ rangeStream sh1 sh2
    where mk is = let v = popCntMemoInt (popCount rs)
                  in  return $ Left (is,v)
          step = undefined
          {-# INLINE [1] mk #-}
          {-# INLINE [1] step #-}
  {-# INLINE topmostIndex #-}
  topmostIndex _ _ = error "topmostIndex/not implemented"



instance Shape PathSet where
  {-# INLINE [1] rank #-}
  rank _ = 1
  {-# INLINE [1] zeroDim #-}
  zeroDim = PathSet 0 0 0
  {-# INLINE [1] unitDim #-}
  unitDim = PathSet 1 1 1
  {-# INLINE [1] intersectDim #-}
  intersectDim = error "sh:.PathSet / intersectDim"
  {-# INLINE [1] addDim #-}
  addDim (PathSet ls lf ll) (PathSet rs rf rl)
    = PathSet (ls `shiftL` popCount rs) (lf+rf) (ll+rl)
  {-# INLINE [1] size #-}
  size (PathSet s _ _)
    = let !p = popCount s in size (Z:.s:.p:.p)
  {-# INLINE [1] sizeIsValid #-}
  sizeIsValid (PathSet p _ _) = True
  {-# INLINE [1] toIndex #-}
  toIndex (PathSet sS fF lL) (PathSet s f l)
    = let !p = popCount sS
      in  toIndex (Z:.sS:.p:.p) (Z:.s:.f:.l)
  {-# INLINE [1] fromIndex #-}
  fromIndex = error "sh:.PathSet / fromIndex"
  {-# INLINE [1] inShapeRange #-}
  inShapeRange = error "sh:.PathSet / inShapeRange"
  {-# NOINLINE listOfShape #-}
  listOfShape = error "sh:.PathSet / listOfShape"
  {-# NOINLINE shapeOfList #-}
  shapeOfList = error "sh:.PathSet / shapeOfList"
  {-# INLINE deepSeq #-}
  deepSeq p x = p `seq` x

instance ExtShape PathSet where
  {-# INLINE [1] subDim #-}
  subDim (PathSet ls lf ll) (PathSet rs rf rl)
    = PathSet (ls `shiftR` popCount rs) (lf-rf) (ll-rl)
  {-# INLINE rangeList #-}
  rangeList _ _ = error "rangeList/not implemented"
  {-# INLINE rangeStream #-}
  -- TODO should be as fast as an @M.unfoldr@?
  rangeStream (PathSet 0 0 0) (PathSet rs rf rl) = M.flatten mk step Unknown $ M.singleton ()
    where mk () = let v = popCntMemoInt (popCount rs)
                  in  return $ Left v
          step (Left !v)
            | G.null v  = return $ M.Done
            | pcnt == 0 = return $ M.Yield (PathSet  0 0 0) $ Left  vt -- only one case
            | pcnt == 1 = return $ M.Yield (PathSet vh l l) $ Left  vt -- only one case
            | otherwise = return $ M.Skip                   $ Right (v,vh,vh `clearBit` l)  -- prepare bit pools
            where pcnt = popCount  vh
                  l    = lsbActive vh
                  vh   = G.unsafeHead v
                  vt   = G.unsafeTail v
          step (Right (!v,!fp,!lp))
            | fp==0     = return $ M.Skip                     $ Left  vt  -- the fp pool is empty, reset everything
            | lp==0     = return $ M.Skip                     $ Right (v,fn,vh `clearBit` lsbActive fn) -- new lp pool with cutten fp
            | otherwise = return $ M.Yield (PathSet vh af al) $ Right (v,fp,lp `clearBit` al) -- just continue with next lp pool element
            where vh = G.unsafeHead v
                  vt = G.unsafeTail v
                  af = lsbActive fp
                  al = lsbActive lp
                  fn = fp `clearBit` af
          {-# INLINE [1] mk #-}
          {-# INLINE [1] step #-}
  {-# INLINE topmostIndex #-}
  topmostIndex _ _ = error "topmostIndex/not implemented"

{-
test :: IO Int
test = M.length $ rangeStream (PathSet 0 0 0) (PathSet (2^14 -1) 0 0)
{-# NOINLINE test #-}
-}

-}

