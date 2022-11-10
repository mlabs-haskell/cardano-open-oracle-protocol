generate-keys:
	openssl genrsa -out resources/key.pem 2048
	openssl req -new -key resources/key.pem -out resources/certificate.csr
	openssl x509 -req -in resources/certificate.csr -signkey resources/key.pem -out resources/certificate.pem -extfile ./resources/ssl-extensions-x509.conf -extensions v3_ca
	openssl x509 -text -in resources/certificate.pem

test-grpc:
	grpcurl -vv -servername localhost:5081 -insecure -import-path ../../coop-proto -proto ../../coop-proto/fact-statement-store-service.proto localhost:5082 coop.FactStatementStore/getFactStatement
