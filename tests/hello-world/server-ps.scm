

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken process-context))
 (import srfi-13)
 (import zmq zsugar zhelpers)

    (define start-server
     (lambda ()
      (zmq-context (ctx)
       (zmq-socket ctx ((responder ZMQ_REP))
        (assert (equal? 0 (zmq_bind responder "tcp://*:5555")))
        (forever
         (print "A client said \n" (s_recv responder))
         (sleep 1)
         (zmq_send responder (location "World") 5 0))))))

 (start-server) ; entry point



