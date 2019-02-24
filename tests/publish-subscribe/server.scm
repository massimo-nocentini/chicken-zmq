 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken random)
  (chicken format)
  (chicken process-context))
 (import srfi-13)
 (import zmq zhelpers)

    (define start-server
     (lambda ()
      (let* ((context (zmq_ctx_new))
             (publisher (zmq_socket context ZMQ_PUB)))
       (assert (equal? 0 (zmq_bind publisher "tcp://*:5556")))
       (let loop ()
        (let* ((zipcode (pseudo-random-integer 100000))
               (temperature (- (pseudo-random-integer 215) 80))
               (relhumidity (+ (pseudo-random-integer 50) 10))
               (update (make-string 20)))
         (s_send publisher (sprintf "~a ~a ~a" 
                            (string-pad (number->string zipcode) 5 #\0) 
                            temperature 
                            relhumidity)))
        (loop))
       (zmq_close publisher)
       (zmq_ctx_destroy context))))

 (start-server) ; entry point
