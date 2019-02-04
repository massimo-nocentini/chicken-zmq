

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zsugar)

    (define start-client
     (lambda ()
      (display "Connecting to hello world server\n")
      (zmq-context (ctx)
       (zmq-socket ctx (requester ZMQ_REQ)
        (zmq_connect requester "tcp://localhost:5555")
        (for-each
         (lambda (i)
          (let* ((chunk-length 10)
                 (buffer (make-string chunk-length)))
           (print* i ": sending \"Hello\"...\n")
           (zmq_send requester (location "Hello") 5 0)
           (zmq_recv requester (location buffer) chunk-length 0)
           (print* i ": received \"" buffer "\"\n")))
         (iota 10))))))

    (start-client) ; entry point

