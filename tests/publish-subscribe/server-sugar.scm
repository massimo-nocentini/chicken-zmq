

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken random)
  (chicken format)
  (chicken process-context))
 (import srfi-13)
 (import zmq zhelpers zsugar)

    (define start-server
     (lambda ()
      (zmq-context (ctx)
       (zmq-socket ctx ((publisher ZMQ_PUB))
        (✓₀ (zmq_bind publisher "tcp://*:5556"))
        (forever
         (let* ((zipcode (pseudo-random-integer 100000))
                (temperature (- (pseudo-random-integer 215) 80))
                (relhumidity (+ (pseudo-random-integer 50) 10))
                (update (sprintf "~a ~a ~a" 
                         (string-pad (number->string zipcode) 5 #\0) 
                         temperature 
                         relhumidity)))
          (s_send publisher update)))))))

 (start-server) ; entry point



