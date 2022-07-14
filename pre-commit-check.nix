{
  src = ./.;
  settings = {
    ormolu.cabalDefaultExtensions = true;
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
