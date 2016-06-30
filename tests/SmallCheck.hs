
module SmallCheck where

import Control.Applicative
import Data.Bits
import Data.List (nub, sort, group)
import Data.Word (Word)
import Debug.Trace
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.SmallCheck

import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Index.Point
import Data.PrimitiveArray.Index.Set
import Data.PrimitiveArray.Index.Class



prop_Point_I_uniqueness (xs :: [PointL I]) = ys == gs
  where ys  = map length . group $ sort xs
        low = pointLI 0
        hig = maximum xs
        ps  = map (linearIndex low hig) xs
        gs  = map length . group $ sort ps



smallcheck_tests = $(testGroupGenerator)

