
hello-world-client-server-test:
	cp ../../src/*.so .
	csc server.scm 
	csc client.scm
	csc server-sugar.scm 
	csc client-sugar.scm
	csc proxy.scm
	csc -s pub-sub-system-components.scm -J
	csc -s pub-sub-system-components.import.scm -J
	csc pub-sub-system.scm
	csc -s xpub-xsub-components.scm -J
	csc -s xpub-xsub-components.import.scm -J
	csc xpub-xsub-system.scm


