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
    #FIXME hunspell.enable = true;
    #FIXME markdownlint.enable = true;
  };

  tools = { };
}
