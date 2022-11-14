{ pkgs
, cardanoCli
, cardanoNode
, coopPabCli
, coopPublisherCli
, jsFsStoreCli
, plutipLocalCluster
, shellHook
}:
pkgs.mkShell {
  packages = with pkgs; [
    jq
    #    json_pp
    sqlite
    protobuf
    protoc-gen-grpc-web
    haskellPackages.proto-lens-protoc
    nodePackages.npm
    nodejs
    grpcui
    grpcurl
    cardanoCli
    cardanoNode
    coopPabCli
    coopPublisherCli
    jsFsStoreCli
    plutipLocalCluster
  ];

  inherit shellHook;
}
