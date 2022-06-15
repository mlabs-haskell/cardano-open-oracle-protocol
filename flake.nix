{
  description = "cardano-open-oracle-protocol";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-language-server.url = "github:haskell/haskell-language-server";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        extra-tools = [
        ];

        pkgs = import nixpkgs { inherit system; inherit (haskell-nix) config; };
        pkgsWithOverlay = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };

        ghcVersion = "922";
        compiler-nix-name = "ghc" + ghcVersion;

        hls = inputs.haskell-language-server.packages.${system}."haskell-language-server-${ghcVersion}";

        overlays = [
          haskell-nix.overlay
          (final: prev: {
            cardano-open-oracle-protocol =
              final.haskell-nix.project' {
                inherit compiler-nix-name;
                src = ./pure-impl;
                shell = {
                  buildInputs = [
                    pkgs.nixpkgs-fmt
                    pkgs.haskellPackages.cabal-fmt
                    pkgs.haskellPackages.fourmolu
                    hls
                  ] ++ extra-tools;
                  tools = {
                    cabal = { };
                    hlint = { };
                  };
                  crossPlatform = [ ];
                };
              };
          })
        ];

        flake = pkgsWithOverlay.cardano-open-oracle-protocol.flake { crossPlatforms = p: [ ]; };
        package = flake.packages."cardano-open-oracle-protocol:exe:cardano-open-oracle-protocol";

      in
      flake // {
        defaultPackage = package;
      });
}
