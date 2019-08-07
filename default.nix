{ mkDerivation, aeson, base, binary, bits, cereal, cereal-vector
, containers, criterion, deepseq, DPutils, hashable, lens
, log-domain, mtl, OrderedBits, primitive, QuickCheck, smallcheck
, stdenv, tasty, tasty-quickcheck, tasty-smallcheck, tasty-th, text
, vector, vector-binary-instances, vector-th-unbox
}:
mkDerivation {
  pname = "PrimitiveArray";
  version = "0.9.1.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bits cereal cereal-vector containers deepseq
    DPutils hashable lens log-domain mtl OrderedBits primitive
    QuickCheck smallcheck text vector vector-binary-instances
    vector-th-unbox
  ];
  testHaskellDepends = [
    aeson base binary bits cereal cereal-vector containers deepseq
    DPutils hashable lens log-domain mtl OrderedBits primitive
    QuickCheck smallcheck tasty tasty-quickcheck tasty-smallcheck
    tasty-th text vector vector-binary-instances vector-th-unbox
  ];
  benchmarkHaskellDepends = [
    aeson base binary bits cereal cereal-vector containers criterion
    deepseq DPutils hashable lens log-domain mtl OrderedBits primitive
    QuickCheck smallcheck text vector vector-binary-instances
    vector-th-unbox
  ];
  homepage = "https://github.com/choener/PrimitiveArray";
  description = "Efficient multidimensional arrays";
  license = stdenv.lib.licenses.bsd3;
}
