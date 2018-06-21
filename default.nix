with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet
    =  (import ../Lib-DPutils).hsSrcSet
    // (import ../Lib-OrderedBits).hsSrcSet
    // {PrimitiveArray = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.PrimitiveArray ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      DPutils
      OrderedBits
    ];
  };
}
