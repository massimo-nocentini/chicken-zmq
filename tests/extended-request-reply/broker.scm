

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken process-context))
 (import srfi-13)
 (import zmq zhelpers zsugar)

    (define (start-broker)
     (zmq-socket ((frontend ZMQ_ROUTER)
                  (backend ZMQ_DEALER))
      (✓₀ (zmq_bind frontend "tcp://*:5559"))
      (✓₀ (zmq_bind backend "tcp://*:5560"))
      (zmq_proxy frontend backend NULL)))

 (start-broker) ; entry point



