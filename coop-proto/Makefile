grpcui:
	grpcui -servername "127.0.0.1" -insecure -proto ./coop.proto localhost:5081

protogen:
	protoc -I . --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
		--haskell_out proto_out \
		fact-statement-store-service.proto cardano.proto tx-builder-service.proto publisher-service.proto
