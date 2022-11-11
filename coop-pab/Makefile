WORKDIR = .coop-pab-cli
RESOURCES = resources

generate-keys:
	openssl genrsa -out $(WORKDIR)/key.pem 2048
	openssl req -new -key $(WORKDIR)/key.pem -out $(WORKDIR)/certificate.csr
	openssl x509 -req -in $(WORKDIR)/certificate.csr -signkey $(WORKDIR)/key.pem -out $(WORKDIR)/certificate.pem -extfile $(RESOURCES)/ssl-extensions-x509.conf -extensions v3_ca
	openssl x509 -text -in $(WORKDIR)/certificate.pem

start-cluster:
	local-cluster --wallet-dir .wallets -n 10 --utxos 5 --chain-index-port 9084

run:
	cabal run coop-pab-cli -- deploy --god-wallet $GOD --aa-wallet $AA
	cabal run coop-pab-cli -- mint-cert-redeemers --cert-rdmr-wallet $CERT_RDMR --cert-rdmrs-to-mint 100
	NOW=$(date +%s) && cabal run coop-pab-cli -- mint-auth --aa-wallet $AA --certificate-valid-from $NOW --certificate-valid-to $(expr $NOW + 100000) --cert-rdmr-ac $CERT_RDMR_AC --auth-wallet $AUTH
	cabal run coop-pab-cli -- tx-builder-grpc --auth-wallet $AUTH --fee-wallet $FEE

get-onchain-time:
	@cabal run coop-pab-cli -- get-state --any-wallet 5e58b477a3e949a4ab8b43062f5981ea7d37319e7fb7b507d994b2af | grep "Current node client time range" | grep POSIXTime | egrep -o "[0-9]+"

test-tx-builder-grpc:
	grpcurl -vv -servername localhost:5081 -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto localhost:5081 coop.TxBuilder/createMintFsTx
	grpcurl -vv -servername localhost:5081 -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto localhost:5081 coop.TxBuilder/createGcFsTx

run-grpcui:
	grpcui -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto localhost:5081

cardano-cli:
	cardano-cli transaction sign --tx-file .coop-pab-cli/signed --signing-key-file .wallets/signing-key-a0e37d3d2a2fbf28e0ffd5cf58ff3593c3e5746bd44e12214fba0b37.skey --out-file .coop-pab-cli/ready
	cardano-cli transaction submit --tx-file .coop-pab-cli/ready  --mainnet

# export GOD=5e58b477a3e949a4ab8b43062f5981ea7d37319e7fb7b507d994b2af AA=533c25f72550f13feea496f534e6f341ba709c1b0ec3727eda19a6bc AUTH=188ba4278cbb1e689bbbc1d5e459c73ff5a7156613871baebcc4b26e FEE=96125265aebc326571e7457ac5f9769d0daa60f8596fa141dce69458 CERT_RDMR=b6a50e493fde97ff3c553dbd1e3f31924d6d61ee1782e40b8b6b967e CARDANO_NODE_SOCKET_PATH=/tmp/test-cluster74839/pool-1/node.socket

# {
#   "factStatements": [
#     {
#       "fsId": "",
#       "gcAfter": {
#         "extended": "NEG_INF"
#       },
#       "fs": {
#         "pdint": "1337"
#       }
#     }
#   ],
#   "submitter": {
#     "base16": "f6db3553be74f82f2240656eaf258f994d926318b802af5df3fe92ee"
#   }
# }
