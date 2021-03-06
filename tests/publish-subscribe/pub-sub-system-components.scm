(module pub-sub-system-components *

 (import scheme
  (chicken base)
  (chicken format)
  (chicken random))
 (import matchable)
 (import srfi-1 srfi-13)
 (import zmq zhelpers zsugar)

 (define zipcode/random
  (lambda ()
   (let ((code (pseudo-random-integer 100000)))
    (string-pad (number->string code) 5 #\0))))

 (define client
  (zmq-component pid (id measures) (channel)
   (zmq-context (ctx)
    (zmq-socket ctx ((subscriber ZMQ_SUB (ZMQ_SUBSCRIBE id)))
     (✓₀ (zmq_connect subscriber (channel->string/connect channel)))
     (let* ((L (lambda (i)
                (match-let (((id₀ temp _) (string-tokenize (s_recv subscriber))))
                 (assert (equal? id id₀)) ; ensure that the message is really for me.
                 (string->number temp))))
            (total-temp (foldr + 0 (map L (iota measures))))
            (average-temp (/ total-temp measures)))
      (print `(average temperature for zipcode ,id was ,average-temp))
      (flush-output))))))

    (define server
     (zmq-component pid () (channel)
      (zmq-context (ctx)
       (zmq-socket ctx ((publisher ZMQ_PUB))
        (✓₀ (zmq_bind publisher (channel->string/bind channel)))
        (forever
         (let* ((temperature (- (pseudo-random-integer 215) 80))
                (relhumidity (+ (pseudo-random-integer 50) 10))
                (recipient (zipcode/random))
                (update (format #f "~a ~a ~a" recipient temperature relhumidity)))
          (s_send publisher update)))))))
    )
