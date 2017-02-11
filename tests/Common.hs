
module Common where

import           Control.Arrow ((&&&), second)
import           Data.List (nub, sort, group)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Data.PrimitiveArray.Index.Class


-- * generic functions

-- | Generates a list of, eg, @PointL@s. This are then grouped according to
-- the @linearIndex@. Within each group, there should only be @PointL@s
-- with the same value.

uniquenessTest :: (Ord a, Index a) => a -> a -> [a] -> Bool
uniquenessTest low hgh xs = all allEq ys && all allEq zs
  where ys  = M.fromListWith S.union . map (second S.singleton) . map (linearIndex low hgh &&& id) $ xs
        zs  = M.fromListWith S.union . map (second S.singleton) . map (id &&& linearIndex low hgh) $ xs
{-# Inlineable uniquenessTest #-}
{-
uniquenessTest low xs = all allEq ys && all allEq zs
  where hgh = maximum xs
        ys  = group . sort . map (linearIndex low hgh &&& id) $ xs
        zs  = group . sort . map (id &&& linearIndex low hgh) $ xs
-}

-- | are all @xs@ equal to each other

allEq = (1==) . S.size
{-# Inline allEq #-}

{-
allEq [] = True
allEq (x:xs) = all (x==) xs
-}


