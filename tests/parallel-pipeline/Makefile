
parallel-pipeline-test:
	cp ../../src/*.so .
	csc ventilator.scm 
	csc worker.scm
	csc sink.scm

run:
	./sink &
	./ventilator &
	./worker α &
	./worker β &
	./worker γ &
	./worker ι &
	./worker μ &

