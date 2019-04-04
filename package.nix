let
  DPutils = (import ../DPutils/package.nix) {};
  OrderedBits = (import ../OrderedBits/package.nix) {};
  lib =
    { src = ./lib;
      dependencies = [
        "aeson"
        "binary"
        "bits"
        "cereal"
        "cereal-vector"
        "DPutils"
        "lens"
        "log-domain"
        "mtl"
        "OrderedBits"
        "QuickCheck"
        "smallcheck"
        "vector"
        "vector-binary-instances"
        "vector-th-unbox"
        ];
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
        ];
      packages = [];
    };
in
  lib
