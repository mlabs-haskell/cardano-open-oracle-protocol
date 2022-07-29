{ pkgs, shellHook }:
pkgs.mkShell {
  packages = with pkgs; [
    protobuf
    protoc-gen-grpc-web
    haskellPackages.proto-lens-protoc
    nodePackages.npm
    nodejs
  ];

  inherit shellHook;
}
