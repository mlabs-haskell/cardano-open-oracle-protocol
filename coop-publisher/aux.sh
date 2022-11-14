function generate-keys {
    local WORKDIR=.coop-publisher-cli
    local RESOURCES=resources
    mkdir $WORKDIR
    openssl genrsa -out $WORKDIR/key.pem 2048
    openssl req -new -key $WORKDIR/key.pem -out $WORKDIR/certificate.csr
    openssl x509 -req -in $WORKDIR/certificate.csr -signkey $WORKDIR/key.pem -out $WORKDIR/certificate.pem -extfile $RESOURCES/ssl-extensions-x509.conf -extensions v3_ca
    openssl x509 -text -in $WORKDIR/certificate.pem
}

SUBMITTER_WALLET=2b0c9f64145896b8da237926a9ee664aed9923b455c7866fa241d218

function coop-mint-fs {
    resp=$(grpcurl -insecure -import-path ../coop-proto -proto ../coop-proto/publisher-service.proto -d @ localhost:5080 coop.publisher.Publisher/createMintFsTx <<EOF
    {
        "fsInfos": [
            {
                "fsId": "$(echo -ne someidB | base64)",
                "gcAfter": {
                    "extended": "NEG_INF"
                }
            }
        ],
        "submitter": {
            "base16": "$SUBMITTER_WALLET"
        }
    }

EOF
        )
    echo $resp
    rawTx=$(echo $resp | jq '.mintFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "Tx BabbageEra"')
    echo $resp | jq '.info'
    echo $resp | jq '.error'
    echo $rawTx > .coop-publisher-cli/signed
}

function run-grpcui {
    make-exports
    grpcui -insecure -import-path ../coop-proto -proto ../coop-proto/publisher-service.proto localhost:5080
}

function cardano-cli-sign {
    cardano-cli transaction sign --tx-file .coop-pab-cli/signed --signing-key-file .wallets/no-plutip-signing-key-$SUBMITTER_WALLET.skey --out-file .coop-pab-cli/ready
}

function cardano-cli-submit {
    cardano-cli transaction submit --tx-file .coop-pab-cli/ready  --mainnet
}
