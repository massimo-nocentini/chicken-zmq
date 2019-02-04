
(import scheme
 (chicken base)
 (chicken process-context)
 (chicken random))
(import srfi-1)
(import zmq zhelpers zsugar)

    (define (start-ventilator task_nbr)
     (zmq-socket ((sender ZMQ_PUSH)
                  (sink ZMQ_PUSH))
      (✓₀ (zmq_bind sender "tcp://*:5557"))
      (✓₀ (zmq_connect sink "tcp://localhost:5558"))
      (print '(Wait for workers to be ready))
      (sleep 20) ; give time to prepare workers
      (print '(Waiting for the Sink to be ready))
      (s_send sink "0")
      (print '(Sending tasks to workers))
      (let ((L (lambda (i) 
                (let ((workload (add1 (pseudo-random-integer 100))))
                 (s_send sender (number->string workload))
                 workload))))
       (print `(Total expected cost of ,(foldr + 0 (map L (iota task_nbr))) msec)))))


(start-ventilator (string->number (car (command-line-arguments))))
