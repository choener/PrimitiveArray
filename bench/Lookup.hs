
module Main where

import Criterion.Main

import Data.PrimitiveArray as PA



go ∷ (Index i) ⇒ Int → Unboxed i Int → i → Int
{-# Inline go #-}
go !c !pa !i = f c i 0
  where f  0 !i !acc = acc
        f !k !i !acc = f (k-1) i (acc + pa ! i)

go1 ∷ Int → Unboxed (Z:.Int) Int → (Z:.Int) → Int
{-# NoInline go1 #-}
go1 = go

go2 ∷ Int → Unboxed (Z:.Int:.Int) Int → (Z:.Int:.Int) → Int
{-# NoInline go2 #-}
go2 = go

go3 ∷ Int → Unboxed (Z:.Int:.Int:.Int) Int → (Z:.Int:.Int:.Int) → Int
{-# NoInline go3 #-}
go3 = go

main ∷ IO ()
main = do
  let !(pa1 ∷ Unboxed (Z:.Int)           Int) = PA.fromAssocs (ZZ:..LtInt 10)                       0 []
  let !(pa2 ∷ Unboxed (Z:.Int:.Int)      Int) = PA.fromAssocs (ZZ:..LtInt 10:..LtInt 10)            0 []
  let !(pa3 ∷ Unboxed (Z:.Int:.Int:.Int) Int) = PA.fromAssocs (ZZ:..LtInt 10:..LtInt 10:..LtInt 10) 0 []
  defaultMain
    [ bgroup "1"
        [ bench "10^0" $ whnf (go1          1 pa1) (Z:.5)
        , bench "10^3" $ whnf (go1       1000 pa1) (Z:.5)
        , bench "10^6" $ whnf (go1    1000000 pa1) (Z:.5)
        , bench "10^9" $ whnf (go1 1000000000 pa1) (Z:.5)
        ]
    , bgroup "2"
        [ bench "      1" $ whnf (go2       1 pa2) (Z:.5:.5)
        , bench "   1000" $ whnf (go2    1000 pa2) (Z:.5:.5)
        , bench "1000000" $ whnf (go2 1000000 pa2) (Z:.5:.5)
        ]
    , bgroup "3"
        [ bench "      1" $ whnf (go3       1 pa3) (Z:.5:.5:.5)
        , bench "   1000" $ whnf (go3    1000 pa3) (Z:.5:.5:.5)
        , bench "1000000" $ whnf (go3 1000000 pa3) (Z:.5:.5:.5)
        ]
    ]
