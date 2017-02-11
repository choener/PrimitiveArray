
module SmallCheck where

import Control.Applicative
import Data.Bits
import Data.List (nub, sort, group, maximumBy)
import Data.Ord (comparing)
import Data.Word (Word)
import Debug.Trace
import Test.SmallCheck
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.TH

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Index.EdgeBoundary
import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Index.Point
import Data.PrimitiveArray.Index.Set
import Data.PrimitiveArray.Index.Subword

import Common



-- * Uniqueness tests. The @xs@ lists are fairly small.

prop_PointL_I_unique (xs :: [PointL I]) = uniquenessTest (pointLI 0) (maximum xs) xs

prop_Subword_I_unique (xs :: [Subword I]) = uniquenessTest (subword 0 0) (maximumBy (comparing fromSubwordSnd) xs) xs

prop_EdgeBoundary_I_unique (xs :: [EdgeBoundary I]) = uniquenessTest (0 :-> 0) (maximumBy (comparing fromEdgeBoundarySnd) xs) xs



smallcheck_tests = $(testGroupGenerator)

