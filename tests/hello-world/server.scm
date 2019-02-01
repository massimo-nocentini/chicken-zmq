

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken process-context))
 (import srfi-13)
 (import zmq)

    (define start-server
     (lambda ()
      (let* ((context (zmq_ctx_new))
             (responder (zmq_socket context ZMQ_REP))
             (rc (zmq_bind responder "tcp://*:5555")))
       (assert (equal? rc 0))
       (let loop ()
        (let* ((chunk-length 10)
               (buffer (make-string chunk-length)))
         (zmq_recv responder (location buffer) chunk-length 0)
         (print* "A client said \"" buffer "\"\n")
         (sleep 1)
         (zmq_send responder (location "World") 5 0))
        (loop)))))

 (start-server) ; entry point



