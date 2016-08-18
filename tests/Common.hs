
module Common where

import Data.List (nub, sort, group)
import Control.Arrow ((&&&))

import Data.PrimitiveArray.Index.Class


-- * generic functions

-- | Generates a list of, eg, @PointL@s. This are then grouped according to
-- the @linearIndex@. Within each group, there should only be @PointL@s
-- with the same value.

uniquenessTest :: (Ord a, Index a) => a -> [a] -> Bool
uniquenessTest low xs = all allEq ys && all allEq zs
  where hgh = maximum xs
        ys  = group . sort . map (linearIndex low hgh &&& id) $ xs
        zs  = group . sort . map (id &&& linearIndex low hgh) $ xs

-- | are all @xs@ equal to each other

allEq [] = True
allEq (x:xs) = all (x==) xs


