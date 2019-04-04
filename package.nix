let
  DPutils = (import ../DPutils/package.nix);
  OrderedBits = (import ../OrderedBits/package.nix);
  lib =
    { src = ./lib;
      dependencies = [
        "aeson"
        "binary"
        "bits"
        "cereal"
        "cereal-vector"
        "lens"
        "log-domain"
        "mtl"
        "QuickCheck"
        "smallcheck"
        "vector"
        "vector-binary-instances"
        "vector-th-unbox"
        # note: these are taken from hackage, the two imports below the let are not even seen
        # we need to use a snack.nix file to modify ghcWithPackages
#        "DPutils"
#        "OrderedBits"
        ] ++ DPutils.dependencies ++ OrderedBits.dependencies;
      extensions = [
        "BangPatterns"
        "CPP"
        "DataKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "MultiParamTypeClasses"
        "PatternGuards"
        "PatternSynonyms"
        "PolyKinds"
        "RankNTypes"
        "RecordWildCards"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TypeApplications"
        "TypeFamilies"
        "TypeOperators"
        "UndecidableInstances"
        "UnicodeSyntax"
        ] ++ DPutils.extensions ++ OrderedBits.extensions;
      # these are my (local) packages
      packages = [
        DPutils
        OrderedBits
        ] ++ DPutils.packages ++ OrderedBits.packages;
    };
in
  lib

