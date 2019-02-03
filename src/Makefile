
all: zmq zhelpers zsugar

zmq:
	csc -s zmq.scm -J -L /usr/local/lib/libzmq.dylib
	csc -s zmq.import.scm -J -L /usr/local/lib/libzmq.dylib

zhelpers:
	csc -s zhelpers.scm -J -L /usr/local/lib/libzmq.dylib
	csc -s zhelpers.import.scm -J -L /usr/local/lib/libzmq.dylib

zsugar:
	csc -s zsugar.scm -J -L /usr/local/lib/libzmq.dylib
	csc -s zsugar.import.scm -J -L /usr/local/lib/libzmq.dylib

