protoc: ../coop.proto
	mkdir generated-pb
	protoc -I .. coop.proto --js_out=import_style=commonjs:./generated-pb
	protoc -I .. coop.proto --grpc-web_out=import_style=commonjs,mode=grpcwebtext:./generated-pb

bundle: client.js
	npm i
	npx webpack ./client.js

serve: bundle
	npx webpack serve --entry ./dist/main.js --static ./

clean:
	rm -fR node_modules || true
	rm -fR dist || true
	rm -fR generated-pb || true

all: clean protoc bundle serve
