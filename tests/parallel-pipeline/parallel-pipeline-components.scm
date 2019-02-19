
(import scheme
 (chicken base)
 (chicken random))
(import srfi-1)
(import zmq zhelpers zsugar)

    (define sink
     (zmq-component pid () ()
      (zmq-socket ((receiver ZMQ_PULL)
                   (controller ZMQ_PUB))
       (✓₀ (zmq_bind receiver "tcp://*:5558"))
       (✓₀ (zmq_bind controller "tcp://*:5559"))
       (s_recv receiver) ; wait for start of batch, ignore the received string.
       (print '(Batch can start))
       (let ((start-time (s_clock))
             (L (lambda (i)
                 (s_recv receiver)
                 (print* (if (zero? (remainder i 10)) ":" ".")))))
        (for-each L (map add1 (iota 100))) ; waits for workers' confirmations
        (print "\n" `(Total elapsed time of ,(- (s_clock) start-time) msec))
        (s_send controller "KILL") ; Send kill signal to workers
       ))))

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

(import scheme
 (chicken base)
 (chicken process-context)
 (chicken random))
(import srfi-1)
(import (chicken bitwise))
(import zmq zhelpers zsugar)

    (define (start-worker name)
     (zmq-socket ((receiver ZMQ_PULL)
                  (sender ZMQ_PUSH)
                  (controller (ZMQ_SUB "KILL")))
      (✓₀ (zmq_connect receiver "tcp://localhost:5557"))
      (✓₀ (zmq_connect sender "tcp://localhost:5558"))
      (✓₀ (zmq_connect controller "tcp://localhost:5559"))
      (forever (break)
       (zmq-poll
        ((receiver ZMQ_POLLIN)
         (let ((workload (s_recv receiver)))
          (print `(Worker ,name will be busy for ,workload msec))
          (s_sleep (string->number workload))
          (s_send sender "")))
        ((controller ZMQ_POLLIN) (break))))))

(start-worker (car (command-line-arguments)))

(import scheme
 (chicken base)
 (chicken process-context)
 (chicken random))
(import srfi-1)
(import zmq zhelpers zsugar)

    (define (start-worker name)
     (zmq-socket ((receiver ZMQ_PULL)
                  (sender ZMQ_PUSH))
      (✓₀ (zmq_connect receiver "tcp://localhost:5557"))
      (✓₀ (zmq_connect sender "tcp://localhost:5558"))
      (forever
       (let ((workload (s_recv receiver)))
        (print `(Worker ,name will be busy for ,workload msec))
        (s_sleep (string->number workload))
        (s_send sender "")))))

(start-worker (car (command-line-arguments)))
