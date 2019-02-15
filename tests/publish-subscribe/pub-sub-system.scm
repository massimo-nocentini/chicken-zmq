

(import scheme (chicken base) (chicken format))
(import srfi-1)
(import zsugar)
(import pub-sub-system-components)

    (let ((channel (make-channel "tcp" "localhost" "5556"))
          (measures 100)
          (clients 100))
     (zmq-system
      (lambda (fork)
       (fork "server" ((server) channel))
       (for-each (lambda (i)
                  (let* ((zipcode (zipcode/random))
                         (client-name (format #f "client-~a" zipcode)))
                   (fork client-name ((client zipcode measures) channel))))
        (iota clients)))
      (lambda (scripter) (scripter "kill-all.sh"))))
