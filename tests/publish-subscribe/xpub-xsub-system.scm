(module wu-spec *

 (import scheme (chicken base) (chicken format))
 (import srfi-1)
 (import zsugar)
 (import xpub-xsub-components)

 (define chan (make-channel "tcp" "localhost" "5556"))
 (define chan₀ (make-channel "tcp" "localhost" "5555"))
 (define measures 3)
 (define clients 5)

 (define-values (fork PIDs-getter outputs-getter) (zmq-system))

 (define start
  (lambda ()
   (fork "proxy" ((proxy) chan₀ chan))
   (fork "server-1" ((server measures) chan₀))
   (fork "server-2" ((server measures) chan₀))
   (for-each (lambda (i)
              (let* ((zipcode (zipcode/random))
                     (client-name (format #f "client-~a" zipcode)))
               (fork client-name ((client zipcode (* 2 measures)) chan))))
    (iota clients))
   (write-bash-script "kill-all.sh" 
    `("kill" ("-9") ,(PIDs-getter number->string))))))

 (import scheme (chicken base) zsugar)

 (start-repl 'wu-spec) 
