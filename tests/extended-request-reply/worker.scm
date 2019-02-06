

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken process-context))
 (import srfi-13)
 (import zmq zhelpers zsugar)

    (define start-worker
     (lambda (id)
      (zmq-socket ((responder ZMQ_REP))
       (✓₀ (zmq_connect responder "tcp://localhost:5560"))
       (forever
        (let ((msg (s_recv responder)))
         (print `(Worker ,id received ,msg and it is going to sleep))
         (sleep 1)
         (s_send responder (string-append "Worker " id ": World!")))))))

 (start-worker (car (command-line-arguments))) ; entry point



