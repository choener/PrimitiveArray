# default.nix

with import <nixpkgs> {};
haskellPackages.extend (haskell.lib.packageSourceOverrides {
  bimaps = ../Lib-bimaps;
  DPutils = ../Lib-DPutils;
  PrimitiveArray = ./.;
})

