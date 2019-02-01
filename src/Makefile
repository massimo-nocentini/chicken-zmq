
all: zmq zhelpers

zmq:
	csc -s zmq.scm -J -L /usr/local/lib/libzmq.dylib
	csc -s zmq.import.scm -J -L /usr/local/lib/libzmq.dylib

zhelpers:
	csc -s zhelpers.scm -J -L /usr/local/lib/libzmq.dylib
	csc -s zhelpers.import.scm -J -L /usr/local/lib/libzmq.dylib

