generate-keys:
	openssl genrsa -out key.pem 2048
	openssl req -new -key key.pem -out certificate.csr
	openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
