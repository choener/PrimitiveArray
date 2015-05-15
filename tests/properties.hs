
module Main where

import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

import qualified  Data.PrimitiveArray.QuickCheck.Index.Set as QCS



-- prop_Fixed_BitSet_setSucc = QCS.prop_Fixed_BitSet_setSucc



main :: IO ()
main = $(defaultMainGenerator)

