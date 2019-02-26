(module xpub-xsub-components *

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
   (zmq-socket ((subscriber ZMQ_SUB (ZMQ_SUBSCRIBE "*")))
    (✓₀ (zmq-connect subscriber channel))
    (let* ((L (lambda (i)
               (print `(Client for zipcode ,id ready to receive ,i th message))
               (let ((update (s_recv subscriber)))
                (match-let (((id₀ sender temp) (string-tokenize update)))
                 (list sender temp)))))
           (total-temp (foldr cons (list) (map L (iota measures)))))
     (print `(average temperature for zipcode ,id was ,total-temp))
     (flush-output)))))

    (define server
     (zmq-component pid (measures) (channel)
      (zmq-socket ((publisher ZMQ_PUB))
       (✓₀ (zmq-connect publisher channel))
       (sleep 1) ; necessary at least we introduce syncronous handshaking.
       (for-each (lambda (i)
                  (let* ((sender (number->string pid))
                         (recipient (zipcode/random))
                         (update (format #f "* ~a ~a" sender recipient)))
                   (print update)
                   (s_send publisher update)))
        (iota measures)))))

    (define proxy
     (zmq-component pid () (frontend-channel backend-channel)
      (zmq-socket ((frontend ZMQ_XSUB)
                   (backend ZMQ_XPUB (ZMQ_XPUB_VERBOSE 1)))
       (✓₀ (zmq-bind frontend frontend-channel))
       (✓₀ (zmq-bind backend backend-channel))
       (zmq_proxy frontend backend NULL))))
)
