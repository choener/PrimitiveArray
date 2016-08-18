
module SmallCheck where

import Control.Applicative
import Data.Bits
import Data.List (nub, sort, group)
import Data.Word (Word)
import Debug.Trace
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.SmallCheck
import Test.SmallCheck

import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Index.Point
import Data.PrimitiveArray.Index.Set
import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Index.Subword

import Common



-- * Uniqueness tests. The @xs@ lists are fairly small.

prop_PointL_I_unique (xs :: [PointL I]) = uniquenessTest (pointLI 0) xs

prop_Subword_I_unique (xs :: [Subword I]) = uniquenessTest (subword 0 0) xs



smallcheck_tests = $(testGroupGenerator)

