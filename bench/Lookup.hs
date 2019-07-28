
module Main where

import Criterion.Main

import Data.PrimitiveArray as PA



go ∷ (Index i) ⇒ Int → Unboxed i Int → i → Int
{-# Inline go #-}
go !c !pa !i = f c 0
  where f  0 !acc = acc
        f !k !acc = f (k-1) (acc + pa ! i)


main ∷ IO ()
main = do
  let !(pa1 ∷ Unboxed (Z:.Int)           Int) = PA.fromAssocs (ZZ:..LtInt 10)                       0 []
  let !(pa2 ∷ Unboxed (Z:.Int:.Int)      Int) = PA.fromAssocs (ZZ:..LtInt 10:..LtInt 10)            0 []
  let !(pa3 ∷ Unboxed (Z:.Int:.Int:.Int) Int) = PA.fromAssocs (ZZ:..LtInt 10:..LtInt 10:..LtInt 10) 0 []
  defaultMain
    [ bgroup "1"
        [ bench "      1" $ whnf (go       1 pa1) (Z:.5)
        , bench "   1000" $ whnf (go    1000 pa1) (Z:.5)
        , bench "1000000" $ whnf (go 1000000 pa1) (Z:.5)
        ]
    , bgroup "2"
        [ bench "      1" $ whnf (go       1 pa2) (Z:.5:.5)
        , bench "   1000" $ whnf (go    1000 pa2) (Z:.5:.5)
        , bench "1000000" $ whnf (go 1000000 pa2) (Z:.5:.5)
        ]
    , bgroup "3"
        [ bench "      1" $ whnf (go       1 pa3) (Z:.5:.5:.5)
        , bench "   1000" $ whnf (go    1000 pa3) (Z:.5:.5:.5)
        , bench "1000000" $ whnf (go 1000000 pa3) (Z:.5:.5:.5)
        ]
    ]
