
all: zmq zhelpers zsugar

zmq:
	csc -s zmq.scm -J -L /usr/local/lib/libzmq.so
	csc -s zmq.import.scm -J -L /usr/local/lib/libzmq.so

zhelpers:
	csc -s zhelpers.scm -J -L /usr/local/lib/libzmq.so
	csc -s zhelpers.import.scm -J -L /usr/local/lib/libzmq.so

zsugar:
	csc -s zsugar.scm -J -L /usr/local/lib/libzmq.so
	csc -s zsugar.import.scm -J -L /usr/local/lib/libzmq.so

