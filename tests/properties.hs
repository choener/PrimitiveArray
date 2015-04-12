
module Main where

import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

-- import qualified Data.Bits.Ordered.QuickCheck as QC



-- prop_PopCountSet = QC.prop_PopCountSet

main :: IO ()
main = $(defaultMainGenerator)

