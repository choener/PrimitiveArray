
module Data.PrimitiveArray.QuickCheck.Index.Set where

import Control.Applicative
import Data.Bits
import Data.Word (Word)
import Debug.Trace
import Test.QuickCheck hiding (Fixed(..), (.&.))

import Data.PrimitiveArray.Index.Set



-- TODO what exactly does the mask fix? Only bits already @1@, or every bit
-- as it is? The mask should actually freeze-fix those bits, where we are
-- set to @1@!

prop_Fixed_BitSet_setSucc (u :: Word, Fixed m s :: Fixed BitSet) = traceShow (tgo, tsu) $ tgo == tsu
  where tgo = go s
        tsu = (getFixed <$> setSucc (Fixed 0 0) (Fixed 0 h) (Fixed m s))
        fb1 = m .&. s -- fixed bits to 1
        fb0 = m .&. complement s  -- fixed bits to 0
        h   = bit (fromIntegral $ u `mod` 8) - 1
        go x -- continue creating successors, until the mask criterion is met (again).
          | Nothing <- ssx = Nothing
          | Just x' <- ssx
          , fb0 == m .&. complement x'
          , fb1 == m .&. x' = traceShow ('j',fb0,fb1,m,x,x') $ Just x'
          | Just x' <- ssx  = traceShow ('g',fb0,fb1,m,x,x') $ go x'
          where ssx = setSucc 0 h x

