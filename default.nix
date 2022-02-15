{ mkDerivation, aeson, base, binary, bits, cereal, cereal-vector
, containers, criterion, deepseq, DPutils, hashable, hashtables
, lens, lib, log-domain, mtl, OrderedBits, primitive, QuickCheck
, smallcheck, tasty, tasty-quickcheck, tasty-smallcheck, tasty-th
, text, unordered-containers, vector, vector-algorithms
, vector-binary-instances, vector-th-unbox
}:
mkDerivation {
  pname = "PrimitiveArray";
  version = "0.10.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bits cereal cereal-vector containers deepseq
    DPutils hashable hashtables lens log-domain mtl OrderedBits
    primitive QuickCheck smallcheck text unordered-containers vector
    vector-algorithms vector-binary-instances vector-th-unbox
  ];
  testHaskellDepends = [
    aeson base binary bits cereal cereal-vector containers deepseq
    DPutils hashable hashtables lens log-domain mtl OrderedBits
    primitive QuickCheck smallcheck tasty tasty-quickcheck
    tasty-smallcheck tasty-th text unordered-containers vector
    vector-algorithms vector-binary-instances vector-th-unbox
  ];
  benchmarkHaskellDepends = [
    aeson base binary bits cereal cereal-vector containers criterion
    deepseq DPutils hashable hashtables lens log-domain mtl OrderedBits
    primitive QuickCheck smallcheck text unordered-containers vector
    vector-algorithms vector-binary-instances vector-th-unbox
  ];
  homepage = "https://github.com/choener/PrimitiveArray";
  description = "Efficient multidimensional arrays";
  license = lib.licenses.bsd3;
}
