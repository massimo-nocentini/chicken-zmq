
hello-world-client-server-test:
	cp ../../src/*.so .
	csc server.scm 
	csc client.scm
	csc server-sugar.scm
	csc client-sugar.scm
	csc server-ps.scm 
	csc client-ps.scm
	csc -s hello-world-system-components.scm -J
	csc -s hello-world-system-components.import.scm -J
	csc hello-world-system.scm



