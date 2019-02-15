
hello-world-client-server-test:
	cp ../../src/*.so .
	csc worker.scm 
	csc client.scm
	csc broker.scm
	csc broker-buggy.scm
	csc -s ext-req-rep-components.scm -J
	csc -s ext-req-rep-components.import.scm -J
	csc ext-req-rep-system.scm



