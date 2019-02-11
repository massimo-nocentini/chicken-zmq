
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
