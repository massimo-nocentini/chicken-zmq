(module wu-spec *

 (import scheme (chicken base) (chicken format))
 (import srfi-1)
 (import zsugar)
 (import pub-sub-system-components)

 (define chan (make-channel "tcp" "localhost" "5556"))
 (define measures 100)
 (define clients 10)

 (define-values (fork PIDs-getter outputs-getter) (zmq-system))

 (define start
  (lambda ()
   (fork "server" ((server) chan))
   (for-each (lambda (i)
              (let* ((zipcode (zipcode/random))
                     (client-name (format #f "client-~a" zipcode)))
               (fork client-name ((client zipcode measures) chan))))
    (iota clients))
   (write-bash-script "kill-all.sh" 
    `("kill" ("-9") ,(PIDs-getter number->string))))))

 (import scheme (chicken base) zsugar)

 (start-repl 'wu-spec) 
