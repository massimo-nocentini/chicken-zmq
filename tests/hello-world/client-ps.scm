

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process) 
  (chicken io)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zsugar zhelpers)

    (define start-client
     (lambda ()
      (display "Connecting to hello world server\n")
      (zmq-context (ctx)
       (zmq-socket ctx ((requester ZMQ_REQ))
        (zmq_connect requester "tcp://localhost:5555")
        (for-each
         (lambda (i)
          (let-values (((in-port out-port pid) (process "ps")))
           (print* i ": sending \"ps\" output...\n")
           (s_send requester (read-string #f in-port))
           (print* i ": received \"" (s_recv requester) "\"\n")))
         (iota 10))))))

    (start-client) ; entry point

