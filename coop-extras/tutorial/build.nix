{ pkgs
, cardanoCli
, cardanoNode
, coopPabCli
, coopPlutusCli
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
    coopPlutusCli
    coopPublisherCli
    jsFsStoreCli
    plutipLocalCluster
  ];

  shellHook = ''
    ${shellHook}
    ln -s ${../../coop-proto} coop-proto
    ln -s ${../../coop-pab/resources} resources
    source ${./aux.bash}
  '';
}
