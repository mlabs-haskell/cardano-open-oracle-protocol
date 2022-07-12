{
  description = "cardano-open-oracle-protocol";
  nixConfig.bash-prompt =
    "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]cardano-open-oracle-protocol \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    http2-grpc-native = {
      url = github:haskell-grpc-native/http2-grpc-haskell;
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , haskell-nix
    , pre-commit-hooks
    , http2-grpc-native
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      inherit self;# To appease nix-linter
      pkgs = import nixpkgs {
        inherit system;
      };
      pkgsWithOverlay = import nixpkgs {
        inherit system;
        inherit (haskell-nix) config;
        overlays = [ haskell-nix.overlay ];
      };

      pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix);
      pre-commit-devShell = pkgs.mkShell {
        inherit (pre-commit-check) shellHook;
      };
      ghcVersion = "8107";
      compiler-nix-name = "ghc" + ghcVersion;

      pureImplProj = import ./pure-impl/pure-impl.nix {
        inherit pkgs compiler-nix-name;
        haskell-nix = pkgsWithOverlay.haskell-nix;
      };
      pureImplFlake = pureImplProj.flake { };
      protoProj = import ./proto/proto.nix {
        inherit pkgs compiler-nix-name http2-grpc-native;
        haskell-nix = pkgsWithOverlay.haskell-nix;
      };
      protoFlake = protoProj.flake { };

    in
    {

      # Standard flake attributes
      packages = pureImplFlake.packages // protoFlake.packages;
      checks = pureImplFlake.checks // protoFlake.checks // pre-commit-check;
      devShells = rec {
        proto = protoFlake.devShell;
        pure-impl = pureImplFlake.devShell;
        pre-commit = pre-commit-devShell;
        default = proto;
      };
    });
}
