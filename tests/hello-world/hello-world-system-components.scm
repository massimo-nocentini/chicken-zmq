(module hello-world-system-components *

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process)
  (chicken format)
  (chicken io)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import spiffy)
 (import zmq zsugar zhelpers)

 (define httpd
   (zmq-component
     pid (port path) ()
     (root-path path)
     (server-port port)
     (start-server)))

 (define client 
   (zmq-component 
     pid (tasks_nbr sh-cmd) (channel)
     (zmq-socket ((requester ZMQ_REQ))
		 (✓₀ (zmq-connect requester channel))
		 (for-each
		   (lambda (i)
		     (let-values (((in-port out-port pid₀) (process sh-cmd)))
		       (format #t "task ~a sends the captured output of cmd `~a` to the server" i sh-cmd)
		       (zmq-send requester (read-string #f in-port))
		       (format #t " and receives \"~a\" back.~%" (s_recv requester))
		       (flush-output)))
		   (iota tasks_nbr)))))

 (define server 
   (zmq-component pid (bytes sleeping) (channel)
		  (zmq-socket ((responder ZMQ_REP))
			      (✓₀ (zmq-bind responder channel))
			      (forever
				(print "A client said: " (string-trim-both (zmq-recv responder bytes)))
				(flush-output)
				(s_sleep sleeping)
				(zmq-send responder "Thanks for your message"))))))
