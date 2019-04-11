(module hello-world-spec *

 (import scheme (chicken base) (chicken format) (chicken process))
 (import srfi-1)
 (import zsugar)
 (import hello-world-system-components)

 ; parameters
 (define chan (make-channel "tcp" "localhost" "5555"))
 (define tasks_nbr 100)
 (define clients 100)

 ; a fresh environment
 (define-values (fork PIDs-getter outputs-getter) (zmq-system))

 ; setting up the system
 (define start
  (lambda ()
   (fork "server" ((server 2048 300) chan))
   (fork "client-top" ((client tasks_nbr "top -n 1 | head -n10") chan))
   (fork "client-echo" ((client tasks_nbr "echo \"Hello Server!!\"") chan))
   (fork "client-date" ((client tasks_nbr "date") chan))
   (fork "httpd" ((httpd 8080 "./") ))
   (write-bash-script "kill-all.sh" `("kill" ("-9") ,(PIDs-getter number->string)))))

 ) ; end of spec module.

 (import scheme (chicken base) zsugar)

 ; start a repl that takes up and running the entire system.
 (start-repl 'hello-world-spec) 
