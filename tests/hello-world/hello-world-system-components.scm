

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
 (import zmq zsugar zhelpers)

 (define (client tasks_nbr sh-cmd)
  (lambda (channel)
   (zmq-socket ((requester ZMQ_REQ))
    (✓₀ (zmq_connect requester
         (channel->string/connect channel)))
    (for-each
     (lambda (i)
      (let-values (((in-port out-port pid) (process sh-cmd)))
       (format #t "task ~a sends the captured output of cmd `~a` to the server" i sh-cmd)
       (zmq-send requester (read-string #f in-port))
       (format #t " and receives \"~a\" back.~%" (s_recv requester))
       (flush-output)))
     (iota tasks_nbr)))))

    (define (server bytes sleeping)
     (lambda (channel)
      (zmq-socket ((responder ZMQ_REP))
       (✓₀ (zmq_bind responder
            (channel->string/bind channel)))
       (forever
        (print "A client said: " (string-trim-both (zmq-recv responder bytes)))
        (flush-output)
        (s_sleep sleeping)
        (zmq-send responder "Thanks for your message")))))
    )



