

(import scheme (chicken base) zsugar)

(module hello-world-spec *

 (import scheme (chicken base) (chicken format))
 (import srfi-1)
 (import zsugar)
 (import hello-world-system-components)

 (define-values (fork PIDs-getter outputs-getter) (zmq-system))

 (define chan (make-channel "tcp" "localhost" "5555"))
 (define tasks_nbr 100)
 (define clients 100)

 (define start
  (lambda ()
   (fork "server" ((server 2048 300) chan))
   (fork "client-top" ((client tasks_nbr "top -n 2 -l 1") chan))
   (fork "client-echo" ((client tasks_nbr "echo \"Hello Server!!\"") chan))
   (fork "client-date" ((client tasks_nbr "date") chan))

 #;(for-each (lambda (i)
            (let ((client-name (format #f "client-~a" (number->string i))))
             (fork client-name (lambda () ((client tasks_nbr) channel)))))
  (iota clients))

 (write-bash-script "kill-all.sh" "kill" (list "-9") (PIDs-getter number->string))))

 (define watcher (tail-watcher 300 outputs-getter))

 )

(start-repl 'hello-world-spec)



