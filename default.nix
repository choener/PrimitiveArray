{ overrideParDir ? null }:

# @overrideParDir@ is either null, to use jus the default packages, or should
# point to our @devel@ or @master@ directory which contains directories of
# in-development packages.

with (import <nixpkgs> {});
with haskell.lib;
with builtins;

let
  # check child directories below this one
  parentContent = readDir overrideParDir;
  # extract sibling folders that contain a default.nix file
  parentDirs = filter (d: pathExists (overrideParDir + ("/" + d + "/default.nix"))) (attrNames parentContent);
  # construct set of names / source directories for override
  hsSrcSet = listToAttrs (map (d: {name = "${d}"; value = overrideParDir + ("/" + d);}) parentDirs);
  # extend the set of packages with source overrides
  hsPkgs = if (isNull overrideParDir) then haskellPackages else haskellPackages.extend (packageSourceOverrides hsSrcSet);
  # name of this module
  this = trace (cabal-install.patches) (baseNameOf ./.);
in
{
  hsShell = hsPkgs.shellFor {
    packages = p: [ p."${this}" ];
    withHoogle = true;
    buildInputs = [
      cabal-install
    ];
  };
  # nix-build -A hsBuild
  # this shall build and put into ./result
  # the result is a typical ./bin/; ./lib/ etc.
  hsBuild = hsPkgs.callCabal2nix "${this}" ./. {};
}
