{ mkDerivation, aeson, base, binary, bits, cereal, cereal-vector
, containers, deepseq, DPutils, hashable, lens, log-domain, mtl
, OrderedBits, primitive, QuickCheck, smallcheck, stdenv, tasty
, tasty-quickcheck, tasty-smallcheck, tasty-th, text, vector
, vector-binary-instances, vector-th-unbox
}:
mkDerivation {
  pname = "PrimitiveArray";
  version = "0.9.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bits cereal cereal-vector deepseq DPutils
    hashable lens log-domain mtl OrderedBits primitive QuickCheck
    smallcheck text vector vector-binary-instances vector-th-unbox
  ];
  testHaskellDepends = [
    base containers QuickCheck smallcheck tasty tasty-quickcheck
    tasty-smallcheck tasty-th
  ];
  homepage = "https://github.com/choener/PrimitiveArray";
  description = "Efficient multidimensional arrays";
  license = stdenv.lib.licenses.bsd3;
}
