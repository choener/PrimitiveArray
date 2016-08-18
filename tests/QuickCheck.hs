
module QuickCheck where

import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Index.Point
import Data.PrimitiveArray.Index.Set
import Data.PrimitiveArray.Index.Subword

import Common



-- * Uniqueness tests

prop_PointL_I_unique (xs :: [PointL I]) = uniquenessTest (pointLI 0) xs

prop_Subword_I_unique (xs :: [Subword I]) = uniquenessTest (subword 0 0) xs



quickcheck_tests = $(testGroupGenerator)

