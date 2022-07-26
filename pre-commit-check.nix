{
  src = ./.;
  settings = {
    # FIXME: https://github.com/cachix/pre-commit-hooks.nix/issues/155
    ormolu.defaultExtensions = [
      "NoStarIsType"
      "BangPatterns"
      "BinaryLiterals"
      "ConstrainedClassMethods"
      "ConstraintKinds"
      "DataKinds"
      "DeriveAnyClass"
      "DeriveDataTypeable"
      "DeriveFoldable"
      "DeriveFunctor"
      "DeriveGeneric"
      "DeriveLift"
      "DeriveTraversable"
      "DerivingStrategies"
      "DerivingVia"
      "DoAndIfThenElse"
      "EmptyCase"
      "EmptyDataDecls"
      "EmptyDataDeriving"
      "ExistentialQuantification"
      "ExplicitForAll"
      "ExplicitNamespaces"
      "FlexibleContexts"
      "FlexibleInstances"
      "ForeignFunctionInterface"
      "GADTSyntax"
      "GeneralisedNewtypeDeriving"
      "HexFloatLiterals"
      "ImportQualifiedPost"
      "InstanceSigs"
      "KindSignatures"
      "LambdaCase"
      "MonomorphismRestriction"
      "MultiParamTypeClasses"
      "NamedFieldPuns"
      "NamedWildCards"
      "NumericUnderscores"
      "OverloadedStrings"
      "PartialTypeSignatures"
      "PatternGuards"
      "PolyKinds"
      "PostfixOperators"
      "RankNTypes"
      "RelaxedPolyRec"
      "ScopedTypeVariables"
      "StandaloneDeriving"
      "StandaloneKindSignatures"
      "TraditionalRecordSyntax"
      "TupleSections"
      "TypeApplications"
      "TypeFamilies"
      "TypeOperators"
      "TypeSynonymInstances"
    ];
  };

  hooks = {
    nixpkgs-fmt.enable = true;
    nix-linter.enable = true;
    cabal-fmt.enable = true;
    fourmolu.enable = true;
    shellcheck.enable = true;
    hlint.enable = true;
    #FIXME(https://github.com/mlabs-haskell/cardano-open-oracle-protocol/issues/11) hunspell.enable = true;
    markdownlint.enable = true;
  };

  tools = { };
}
