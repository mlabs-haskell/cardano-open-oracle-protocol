{
  description = "cardano-open-oracle-protocol";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    haskell-nix,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system: let
      extra-tools = [];

      pkgs = import nixpkgs {
        inherit system;
        inherit (haskell-nix) config;
      };
      pkgsWithOverlay = import nixpkgs {
        inherit system overlays;
        inherit (haskell-nix) config;
      };
      ghcVersion = "921";
      compiler-nix-name = "ghc" + ghcVersion;

      hls =
        pkgs.haskell-language-server.override
        {
          supportedGhcVersions = [ghcVersion];
        };

      pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
        src = ./.;
        settings = {
          ormolu.defaultExtensions = [
            "TypeApplications"
            "QualifiedDo"
          ];
        };
        hooks = {
          alejandra.enable = true;
          cabal-fmt.enable = true;
          fourmolu.enable = true;
        };
      };

      overlays = [
        haskell-nix.overlay
        (final: prev: {
          cardano-open-oracle-protocol = final.haskell-nix.project' {
            inherit compiler-nix-name;
            src = ./pure-impl;
            shell = {
              buildInputs =
                [
                  pkgs.nixpkgs-fmt
                  pkgs.haskellPackages.cabal-fmt
                  pkgs.haskellPackages.fourmolu
                  hls
                ]
                ++ extra-tools;
              tools = {
                cabal = {};
                hlint = {};
              };
              crossPlatform = [];
              inherit (pre-commit-check) shellHook;
            };
          };
        })
      ];

      flake = pkgsWithOverlay.cardano-open-oracle-protocol.flake {crossPlatforms = p: [];};

      package = flake.packages."cardano-open-oracle-protocol:exe:cardano-open-oracle-protocol";

      # the nix code formatter for nix fmt
      formatter = pkgs.alejandra;

      checks = {inherit pre-commit-check;};
    in
      flake
      // {
        defaultPackage = package;
        devShell = flake.devShell;
        inherit formatter;
        inherit checks;
      });
}
