
module Main where

import Control.Applicative
import Data.Bits
import Data.List (nub, sort, group)
import Data.Word (Word)
import Test.Tasty
import Test.Tasty.TH
import qualified Test.QuickCheck as QC

import Data.PrimitiveArray.Index.IOC
import Data.PrimitiveArray.Index.Point
--import Data.PrimitiveArray.Index.Set
import Data.PrimitiveArray.Index.Class

import QuickCheck
import SmallCheck



-- * Points

-- | @linearIndex <-> fromLinearIndex@

prop_FromLinear_ZP ( x :: PointL I, a')
  | ix == frm = True
  | otherwise = error $ show (x,a',lt, ix, lin, frm)
  where ltx = LtPointL $ QC.getNonNegative a' + fromPointL x
        lt  = ZZ:..ltx
        ix  = Z:.x
        lin = linearIndex lt ix
        frm = fromLinearIndex lt lin

prop_FromLinear_ZPP ( x :: PointL I, y :: PointL I, a', b')
  | ix == frm = True
  | otherwise = error $ show (x,y,a',b',lt, ix, lin, frm)
  where ltx = LtPointL $ QC.getNonNegative a' + fromPointL x
        lty = LtPointL $ QC.getNonNegative b' + fromPointL y
        lt  = ZZ:..ltx:..lty
        ix  = Z:.x:.y
        lin = linearIndex lt ix
        frm = fromLinearIndex lt lin

-- * Sets

-- TODO what exactly does the mask fix? Only bits already @1@, or every bit
-- as it is? The mask should actually freeze-fix those bits, where we are
-- set to @1@!

--prop_Fixed_BitSet_setSucc (u :: Word, Fixed m s :: Fixed (BitSet I)) = traceShow (tgo, tsu) $ tgo == tsu
--  where tgo = go s
--        tsu = (getFixed <$> setSucc (Fixed 0 0) (Fixed 0 h) (Fixed m s))
--        fb1 = m .&. s -- fixed bits to 1
--        fb0 = m .&. complement s  -- fixed bits to 0
--        h   = bit (fromIntegral $ u `mod` 8) - 1
--        go x -- continue creating successors, until the mask criterion is met (again).
--          | Nothing <- ssx = Nothing
--          | Just x' <- ssx
--          , fb0 == m .&. complement x'
--          , fb1 == m .&. x' = traceShow ('j',fb0,fb1,m,x,x') $ Just x'
--          | Just x' <- ssx  = traceShow ('g',fb0,fb1,m,x,x') $ go x'
--          where ssx = setSucc 0 h x



main :: IO ()
main = do
  defaultMain $ testGroup ""
    [ -- quickcheck_tests
--    , smallcheck_tests
    ]

