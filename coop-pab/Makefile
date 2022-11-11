WORKDIR = .coop-pab-cli
RESOURCES = resources

generate-keys:
	openssl genrsa -out $(WORKDIR)/key.pem 2048
	openssl req -new -key $(WORKDIR)/key.pem -out $(WORKDIR)/certificate.csr
	openssl x509 -req -in $(WORKDIR)/certificate.csr -signkey $(WORKDIR)/key.pem -out $(WORKDIR)/certificate.pem -extfile $(RESOURCES)/ssl-extensions-x509.conf -extensions v3_ca
	openssl x509 -text -in $(WORKDIR)/certificate.pem

test-tx-builder-grpc:
	grpcurl -vv -servername localhost:5081 -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto localhost:5081 coop.TxBuilder/createMintFsTx
	grpcurl -vv -servername localhost:5081 -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto localhost:5081 coop.TxBuilder/createGcFsTx
