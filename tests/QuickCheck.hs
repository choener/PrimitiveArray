
module QuickCheck where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Data.PrimitiveArray.Index.Class
--import Data.PrimitiveArray.Index.EdgeBoundary
import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Index.Point
--import Data.PrimitiveArray.Index.Set
--import Data.PrimitiveArray.Index.Subword

import Common



-- * Uniqueness tests

-- prop_PointL_I_unique (xs :: [PointL I]) = uniquenessTest (LtPointL 0) (LtPointL $ maximum $ map fromPointL xs) xs

-- prop_Subword_I_unique (xs :: [Subword I]) = uniquenessTest (subword 0 0) (maximumBy (comparing fromSubwordSnd) xs) xs

-- prop_EdgeBoundary_I_unique (xs :: [EdgeBoundary I]) = uniquenessTest (0 :-> 0) (maximumBy (comparing fromEdgeBoundarySnd) xs) xs

-- | TODO check that bitsets produce the correct number of bits when counting

--prop_BitSet1_First_I_set (numberOfBits ∷ ()) = strm == lst
--  where strm = sort . unId $ streamUp (LtBitSet1 0) (LtBitSet1 0) :: IO [BitSet1 First I]
--        lst  = sort []

quickcheck_tests = $(testGroupGenerator)

