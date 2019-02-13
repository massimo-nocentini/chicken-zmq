

(import scheme (chicken base) (chicken format))
(import srfi-1)
(import zsugar)
(import hello-world-system-components)

(let ((channel (make-channel "tcp" "localhost" "5555"))
      (tasks_nbr 10)
      (clients 10))

    #;(zmq-system 
     (lambda (fork)
      (for-each (lambda (i) 
                 (let ((client-name (format #f "client-~a" (number->string i))))
                  (fork client-name (lambda () ((client tasks_nbr) channel)))))
       (iota clients))
      (fork "server" (lambda () ((server) channel))))
     (lambda (scripter) (scripter "kill-all.sh")))

    (zmq-system 
     (lambda (fork)
      (fork "server" (lambda () ((server 2048) channel)))
      (fork "client-top" (lambda () ((client tasks_nbr "top -n 2 -l 1") channel)))
      (fork "client-date" (lambda () ((client tasks_nbr "date") channel))))
     (lambda (scripter) (scripter "kill-all.sh")))

    )
