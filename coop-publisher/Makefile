generate-keys:
	openssl genrsa -out key.pem 2048
	openssl req -new -key key.pem -out certificate.csr
	openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem -extfile ssl-extensions-x509.conf -extensions v3_ca
	openssl x509 -text -in certificate.pem
