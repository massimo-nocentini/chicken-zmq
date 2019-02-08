
parallel-pipeline-test:
	cp ../../src/*.so .
	csc ventilator.scm 
	csc worker.scm
	csc sink.scm
	csc worker-with-signaling.scm
	csc sink-with-signaling.scm

