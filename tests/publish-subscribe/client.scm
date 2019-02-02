

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zhelpers)

    (define start-client
     (lambda (filter)
      (display "Collecting updates from weather server\n")
      (let* ((context (zmq_ctx_new))
             (subscriber (zmq_socket context ZMQ_SUB))
             (L (lambda (i)
                 (let ((str (s_recv subscriber)))
                  (string->number (cadr (string-tokenize str)))))))
       (assert 
        (equal? 0 (zmq_setsockopt subscriber ZMQ_SUBSCRIBE 
                   (location filter) 
                   (string-length filter))))
       (assert 
        (equal? 0 (zmq_connect subscriber "tcp://localhost:5556")))
       (let* ((measures 100)
              (total-temp (foldr + 0 (map L (iota measures))))
              (average-temp (/ total-temp measures)))
        (print* `(average temperature for zipcode ,filter was ,average-temp) "\n"))
       (zmq_close subscriber)
       (zmq_ctx_destroy context))))

    (start-client (car (command-line-arguments))) ; entry point
