generate-keys:
	openssl genrsa -out .json-fs-store/key.pem 2048
	openssl req -new -key .json-fs-store/key.pem -out .json-fs-store/certificate.csr
	openssl x509 -req -in .json-fs-store/certificate.csr -signkey .json-fs-store/key.pem -out .json-fs-store/certificate.pem -extfile ./resources/ssl-extensions-x509.conf -extensions v3_ca
	openssl x509 -text -in .json-fs-store/certificate.pem

genesis:
	rm .json-fs-store/json-store.db || true
	sqlite3 -batch .json-fs-store/json-store.db ""
	cabal run json-fs-store-cli -- genesis --db .json-fs-store/json-store.db

serve:
	cabal run json-fs-store-cli -- fact-statement-store-grpc --db .json-fs-store/json-store.db

insert:
	cabal run json-fs-store-cli -- insert-fact-statement --db .json-fs-store/json-store.db --fact_statement_id "someidA" --json "[1,2,3]"
	cabal run json-fs-store-cli -- insert-fact-statement --db .json-fs-store/json-store.db --fact_statement_id "someidB" --json "[1,2,3]"
	cabal run json-fs-store-cli -- insert-fact-statement --db .json-fs-store/json-store.db --fact_statement_id "someidC" --json "[1,2,3]"

showdb:
	echo "SELECT * FROM fact_statements" | sqlite3 .json-fs-store/json-store.db

test-grpc:
	grpcurl -vv -servername localhost:5081 -insecure -import-path ../../coop-proto -proto ../../coop-proto/fact-statement-store-service.proto -d '{"fsIds": [{ "value": "c29tZWlkQQ==" }, { "value": "c29tZWlkQg==" }, { "value": "c29tZWlkQw==" }]}' localhost:5082 coop.FactStatementStore/getFactStatement

run-grpcui:
	grpcui -insecure -import-path ../../coop-proto -proto ../../coop-proto/fact-statement-store-service.proto localhost:5082
