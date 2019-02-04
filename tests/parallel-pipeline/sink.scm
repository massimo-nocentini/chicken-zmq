
(import scheme
 (chicken base)
 (chicken process-context)
 (chicken random))
(import srfi-1)
(import zmq zhelpers zsugar)

    (define (start-sink)
     (zmq-socket ((receiver ZMQ_PULL))
      (✓₀ (zmq_bind receiver "tcp://*:5558"))
      (s_recv receiver) ; wait for start of batch
      (print '(Batch can start))
      (let ((start-time (s_clock))
            (L (lambda (i)
                (s_recv receiver)
                (print* (if (zero? (remainder i 10)) ":" ".")))))
       (for-each L (map add1 (iota 100))) ; waits for workers' confirmations
       (print "\n" `(Total elapsed time of ,(- (s_clock) start-time) msec)))))

(start-sink)
