
module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import qualified  Data.PrimitiveArray.QuickCheck.Index.Set as QCS



-- prop_Fixed_BitSet_setSucc = QCS.prop_Fixed_BitSet_setSucc



main :: IO ()
main = $(defaultMainGenerator)

