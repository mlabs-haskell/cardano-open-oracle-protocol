# shellcheck disable=SC2155,SC2002,SC2003
function generate-keys {
    local WORKDIR=.coop-pab-cli
    local RESOURCES=resources
    openssl genrsa -out $WORKDIR/key.pem 2048
    openssl req -new -key $WORKDIR/key.pem -out $WORKDIR/certificate.csr
    openssl x509 -req -in $WORKDIR/certificate.csr -signkey $WORKDIR/key.pem -out $WORKDIR/certificate.pem -extfile $RESOURCES/ssl-extensions-x509.conf -extensions v3_ca
    openssl x509 -text -in $WORKDIR/certificate.pem
}

function start-cluster {
    # As specified in resources/pabConfig.yaml
    rm -fR .wallets
    rm -fR .local-cluster
    mkdir .wallets
    mkdir .local-cluster
    mkdir .local-cluster/txs
    mkdir .local-cluster/scripts
    local-cluster --wallet-dir .wallets -n 10 --utxos 5 --chain-index-port 9084 --slot-len 1s --epoch-size 100000
}

function parse-cluster-config {
    cat > .coop-pab-cli/plutip-cluster-config
    make-exports
    mv .wallets/signing-key-"$SUBMITTER_PKH".skey .wallets/no-plutip-signing-key-"$SUBMITTER_PKH".skey
}

function make-exports {
    export GOD_PKH=$(cat .coop-pab-cli/plutip-cluster-config | grep -E "Wallet 1 PKH" | cut -d ":" -f 2 | xargs)
    export AA_PKH=$(cat .coop-pab-cli/plutip-cluster-config | grep -E "Wallet 2 PKH" | cut -d ":" -f 2 | xargs)
    export AUTH_PKH=$(cat .coop-pab-cli/plutip-cluster-config | grep -E "Wallet 3 PKH" | cut -d ":" -f 2 | xargs)
    export CERT_RDMR_PKH=$(cat .coop-pab-cli/plutip-cluster-config | grep -E "Wallet 4 PKH" | cut -d ":" -f 2 | xargs)
    export FEE_PKH=$(cat .coop-pab-cli/plutip-cluster-config | grep -E "Wallet 5 PKH" | cut -d ":" -f 2 | xargs)
    export SUBMITTER_PKH=$(cat .coop-pab-cli/plutip-cluster-config | grep -E "Wallet 6 PKH" | cut -d ":" -f 2 | xargs)
    export CARDANO_NODE_SOCKET_PATH=$(cat .coop-pab-cli/plutip-cluster-config | grep CardanoNodeConn | grep -E -o '"[^"]+"' | sed s/\"//g)
}

function dump-env {
    export | grep -E "WALLET|CARDANO_NODE_SOCKET_PATH"
}

function coop-genesis {
    make-exports
    cabal clean
    cabal run coop-pab-cli -- deploy --god-wallet "$GOD_PKH" --aa-wallet "$AA_PKH"
}

function coop-mint-cert-redeemers {
    make-exports
    cabal run coop-pab-cli -- mint-cert-redeemers --cert-rdmr-wallet "$CERT_RDMR_PKH" --cert-rdmrs-to-mint 100
}

function coop-mint-authentication {
    make-exports
    NOW=$(get-onchain-time) && cabal run coop-pab-cli -- mint-auth --aa-wallet "$AA_PKH" --certificate-valid-from "$NOW" --certificate-valid-to "$(expr "$NOW" + 60 \* 60 \* 1000)" --auth-wallet "$AUTH_PKH"
}

function coop-redist-auth {
    make-exports
    cabal run coop-pab-cli -- redistribute-auth --auth-wallet "$AUTH_PKH"
}

function coop-run-tx-builder-grpc {
    make-exports
    cabal run coop-pab-cli -- tx-builder-grpc --auth-wallet "$AUTH_PKH" --fee-wallet "$FEE_PKH"
}

function coop-garbage-collect {
    make-exports
    cabal run coop-pab-cli -- garbage-collect --cert-rdmr-wallet "$CERT_RDMR_PKH"
}

function coop-get-state {
    make-exports
    cabal run coop-pab-cli -- get-state --any-wallet "$GOD_PKH"
    cat .coop-pab-cli/coop-state.json | json_pp
}

function coop-poll-state {
    make-exports
    while true; do
        clear;
        coop-get-state;
        sleep 5;
    done;
}

function get-onchain-time {
    make-exports
    cabal run coop-pab-cli -- get-state --any-wallet "$GOD_PKH" | grep "Current node client time range" | grep POSIXTime | grep -E -o "[0-9]+"
}

function run-grpcui {
    make-exports
    grpcui -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto localhost:5081
}

function coop-mint-fs {
    make-exports
    resp=$(grpcurl -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto -d @ localhost:5081 coop.tx_builder.TxBuilder/createMintFsTx <<EOF
{
  "factStatements": [
    {
      "fsId": "$(echo -ne 'the best id1' | base64)",
      "gcAfter": {
        "extended": "NEG_INF"
      },
      "fs": {
        "pdint": "1337"
      }
    },
    {
      "fsId": "$(echo -ne 'the best id2' | base64)",
      "gcAfter": {
        "extended": "NEG_INF"
      },
      "fs": {
        "pdbytes": "$(echo -ne 'some bytes' | base64)"
      }
    },
    {
      "fsId": "$(echo -ne 'the best id3' | base64)",
      "gcAfter": {
        "extended": "NEG_INF"
      },
      "fs": {
        "pdlist": {
          "elements": [
            {
              "pdint": "1337"
            }
          ]
        }
      }
    },
    {
      "fsId": "$(echo -ne 'the best id4' | base64)",
      "gcAfter": {
        "extended": "FINITE",
        "finiteLedgerTime": "$(expr "$(get-onchain-time)" + 60 \* 60 \* 1000)"
      },
      "fs": {
        "pdlist": {
          "elements": [
            {
              "pdint": "1337"
            }
          ]
        }
      }
    },
    {
      "fsId": "$(echo -ne 'the best id5' | base64)",
      "gcAfter": {
        "extended": "FINITE",
        "finiteLedgerTime": "$(expr "$(get-onchain-time)" + 60 \* 60 \* 1000)"
      },
      "fs": {
        "pdlist": {
          "elements": [
            {
              "pdint": "1337"
            }
          ]
        }
      }
    }
  ],
  "submitter": {
    "base16": "$SUBMITTER_PKH"
  }
}
EOF
           )
    rawTx=$(echo "$resp" | jq '.success.mintFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "Tx BabbageEra"')
    echo "$resp" | jq '.info'
    echo "$resp" | jq '.error'
    echo "$rawTx" > .coop-pab-cli/signed
    cardano-cli transaction sign --tx-file .coop-pab-cli/signed --signing-key-file .wallets/no-plutip-signing-key-"$SUBMITTER_PKH".skey --out-file .coop-pab-cli/ready
    cardano-cli transaction submit --tx-file .coop-pab-cli/ready  --mainnet
}

function coop-gc-fs {
    make-exports
    resp=$(grpcurl -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto -d @ localhost:5081 coop.tx_builder.TxBuilder/createGcFsTx <<EOF
    {
        "fsIds": [
                 "$(echo -ne 'the best id1' | base64)",
                 "$(echo -ne 'the best id2' | base64)",
                 "$(echo -ne 'the best id3' | base64)",
                 "$(echo -ne 'the best id4' | base64)",
                 "$(echo -ne 'the best id5' | base64)"
                 ],
        "submitter": {
            "base16": "$SUBMITTER_PKH"
        }
    }

EOF
        )
    rawTx=$(echo "$resp" | jq '.success.gcFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "TxBodyBabbage"')
    echo "$resp" | jq '.info'
    echo "$resp" | jq '.error'
    echo "$rawTx" > .coop-pab-cli/signed
    cardano-cli transaction sign --tx-body-file .coop-pab-cli/signed --signing-key-file .wallets/no-plutip-signing-key-"$SUBMITTER_PKH".skey --out-file .coop-pab-cli/ready
    cardano-cli transaction submit --tx-file .coop-pab-cli/ready  --mainnet
}

function coop-prelude {
    make-exports
    coop-genesis
    coop-mint-cert-redeemers
    coop-mint-authentication
    coop-redist-auth
    coop-run-tx-builder-grpc
}
