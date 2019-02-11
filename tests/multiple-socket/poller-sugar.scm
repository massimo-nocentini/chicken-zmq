

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken bitwise)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zhelpers zsugar)

    (define (start-poller id)
     (zmq-socket ((receiver ZMQ_PULL)
                  (subscriber (ZMQ_SUB id)))
      (✓₀ (zmq_connect receiver "tcp://localhost:5557"))
      (✓₀ (zmq_connect subscriber "tcp://localhost:5556"))
      (forever
       (zmq-poll
        ((receiver ZMQ_POLLIN) 
         (let ((msg (s_recv receiver)))
          (print `(received from ventilator the workload ,(string-trim-both msg)))))
        ((subscriber ZMQ_POLLIN) 
         (let ((msg (s_recv subscriber)))
          (print `(received from weather publisher the temperature ,(string-trim-both msg)))))))))

    (start-poller (car (command-line-arguments))) ; entry point
