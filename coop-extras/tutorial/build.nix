{ pkgs
, cardanoCli
, cardanoNode
, coopPabCli
, coopPlutusCli
, coopPublisherCli
, jsFsStoreCli
, plutipLocalCluster
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
    ln -s ${../../coop-proto} coop-proto
    ln -s ${../../coop-pab/resources} resources
    . ${./aux.bash}
    echo "WARNING: Running COOP services requires having $ export LC_CTYPE=C.UTF-8 LC_ALL=C.UTF-8 LANG=C.UTF-8"
    export LC_CTYPE=C.UTF-8
    export LC_ALL=C.UTF-8
    export LANG=C.UTF-8
  '';
}
