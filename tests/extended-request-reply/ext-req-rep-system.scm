

(import scheme (chicken base) (chicken format))
(import srfi-1)
(import zsugar)
(import ext-req-rep-components)

(let ((frontend-channel (make-channel "tcp" "localhost" "5559"))
      (backend-channel (make-channel "tcp" "localhost" "5560"))
      (tasks_nbr 100)
      (clients 100)
      (workers 100))

    (zmq-system
     (lambda (fork)
      ;(fork "broker-poll" ((broker/poll) frontend-channel backend-channel)) ; it works even though the following
      (fork "broker-proxy" ((broker/proxy) frontend-channel backend-channel)) ; is far more performant.
      (for-each (lambda (i)
                 (let ((client-name (format #f "client-~a" (number->string i))))
                  (fork client-name ((client tasks_nbr) frontend-channel))))
       (iota clients))
      (for-each (lambda (i)
                 (let ((worker-name (format #f "worker-~a" (number->string i))))
                  (fork worker-name ((worker) backend-channel))))
       (iota workers)))
     (lambda (scripter) (scripter "kill-all.sh")))

    )
