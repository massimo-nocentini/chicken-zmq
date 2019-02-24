 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zhelpers zsugar)

    (define start-client
     (lambda (filter)
      (display "Collecting updates from weather server\n")
      (zmq-context (ctx)
       (zmq-socket ctx ((subscriber (ZMQ_SUB filter)))
        (✓₀ (zmq_connect subscriber "tcp://localhost:5556"))
        (let* ((L (lambda (i)
                   (let ((str (s_recv subscriber)))
                    (string->number (cadr (string-tokenize str))))))
               (measures 100)
               (total-temp (foldr + 0 (map L (iota measures))))
               (average-temp (/ total-temp measures)))
         (print* `(average temperature for zipcode ,filter was ,average-temp) "\n"))))))

    (start-client (car (command-line-arguments))) ; entry point
