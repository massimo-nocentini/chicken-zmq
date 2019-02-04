

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken bitwise)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zhelpers zsugar)

    (define (start-poller id)
     (zmq-socket ((receiver ZMQ_PULL)
                  (subscriber (ZMQ_SUB id)))
      (✓₀ (zmq_connect receiver "tcp://localhost:5557"))
      (✓₀ (zmq_connect subscriber "tcp://localhost:5556"))
      (forever
       (let ((items (list 
                     (make-zmq_pollitem_t receiver 0 ZMQ_POLLIN 0)
                     (make-zmq_pollitem_t subscriber 0 ZMQ_POLLIN 0)))
             (msg (make-string 255)))
        (zmq_poll (location (list->vector items)) 2 -1)
        (when (bitwise-and (zmq_pollitem_t-revents (car items)) ZMQ_POLLIN) 
         (let ((size (zmq_recv receiver (location msg) 255 0)))
          (unless (equal? -1 size)
           (print `(received from ventilator the workload ,(string-trim-both msg))))))
        (when (bitwise-and (zmq_pollitem_t-revents (cadr items)) ZMQ_POLLIN) 
         (let ((size (zmq_recv subscriber (location msg) 255 0)))
          (unless (equal? -1 size)
           (print `(received from weather publisher the temperature ,(string-trim-both msg))))))))))

    (start-poller (car (command-line-arguments))) ; entry point
