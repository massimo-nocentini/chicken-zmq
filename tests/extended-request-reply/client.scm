

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zhelpers zsugar)

    (define start-client
     (lambda (id)
      (zmq-socket ((requester ZMQ_REQ))
       (zmq_connect requester "tcp://localhost:5559")
       (for-each
        (lambda (i)
         (s_send requester (string-append "Client " id ": Hello!"))
         (let ((response (s_recv requester))
               (ith (symbol-append (string->symbol i) '-th)))
          (print `(Client ,id receives its ,ith response from ,response))))
        (map number->string (iota 10))))))

    (start-client (car (command-line-arguments))) ; entry point

