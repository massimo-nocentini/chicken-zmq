 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken process-context))
 (import srfi-13)
 (import zmq zsugar)

    (define start-server
     (lambda ()
      (zmq-context (ctx)
       (zmq-socket ctx ((responder ZMQ_REP))
        (assert (equal? 0 (zmq_bind responder "tcp://*:5555")))
        (forever
         (let* ((chunk-length 10)
                (buffer (make-string chunk-length)))
          (zmq_recv responder (location buffer) chunk-length 0)
          (print* "A client said \"" buffer "\"\n")
          (sleep 1)
          (zmq_send responder (location "World") 5 0)))))))

 (start-server) ; entry point
