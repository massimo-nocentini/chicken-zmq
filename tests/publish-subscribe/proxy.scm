

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken process-context))
 (import srfi-13)
 (import zmq zhelpers zsugar)

    (define (start-proxy)
     (zmq-socket ((frontend ZMQ_XSUB)
                  (backend ZMQ_XPUB))
      (✓₀ (zmq_connect frontend "tcp://192.168.55.210:5556"))
      (✓₀ (zmq_bind backend "tcp://10.1.1.0:8100"))
      (zmq_proxy frontend backend NULL)))

 (start-proxy) ; entry point



