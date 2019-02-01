

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq)

    (define start-client
     (lambda ()
      (display "Connecting to hello world server\n")
      (let* ((context (zmq_ctx_new))
             (requester (zmq_socket context ZMQ_REQ))
             (L (lambda (i)
                 (let* ((chunk-length 10)
                        (buffer (make-string chunk-length)))
                  (print* i ": sending \"Hello\"...\n")
                  (zmq_send requester (location "Hello") 5 0)
                  (zmq_recv requester (location buffer) chunk-length 0)
                  (print* i ": received \"" buffer "\"\n")))))
       (zmq_connect requester "tcp://localhost:5555")
       (for-each L (iota 10))
       (zmq_close requester)
       (zmq_ctx_destroy context))))

    (start-client) ; entry point
