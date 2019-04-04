
let
  DPutils = (import ../DPutils/package.nix);
  OrderedBits = (import ../OrderedBits/package.nix);
  PrimitiveArray = (import ./package.nix);
in
  { main = "properties";
    src = ./tests;
    packages = [ PrimitiveArray ];
    extensions = PrimitiveArray.extensions;
    dependencies = [
      "QuickCheck"
      "smallcheck"
      "tasty"
      "tasty-quickcheck"
      "tasty-smallcheck"
      "tasty-th"
      ];
  }

