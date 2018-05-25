with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    DPutils = ../Lib-DPutils;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
  });
  hsShell = hsPkgs.shellFor {
    packages = p: [ p.DPutils p.OrderedBits p.PrimitiveArray ];
    withHoogle = true;
    buildInputs = [ cabal-install ghc ];
  };
}
