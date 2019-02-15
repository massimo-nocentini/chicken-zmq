

(module ext-req-rep-components *

 (import scheme (chicken base))
 (import srfi-1 srfi-13)
 (import zmq zhelpers zsugar)

    (define client 
     (zmq-component pid (tasks_nbr) (channel)
      (zmq-socket ((requester ZMQ_REQ))
       (✓₀ (zmq-connect requester channel))
       (for-each
        (lambda (i)
         (s_send requester (string-append "Client " (number->string pid) ": Hello!"))
         (let ((response (s_recv requester))
               (ith (symbol-append (string->symbol i) '-th)))
          (print `(Client ,pid receives its ,ith response from ,response))
          (flush-output)))
        (map number->string (iota tasks_nbr))))))

    (define worker
     (zmq-component pid () (channel)
      (zmq-socket ((responder ZMQ_REP))
       (✓₀ (zmq-connect responder channel))
       (forever
        (let ((msg (s_recv responder)))
         (print `(Worker ,pid received ,msg and it is going to sleep))
         (flush-output)
         (sleep 1)
         (s_send responder (string-append "Worker " (number->string pid) ": World!")))))))

    (define broker/proxy
     (zmq-component pid () (frontend-channel backend-channel)
      (zmq-socket ((frontend ZMQ_ROUTER)
                   (backend ZMQ_DEALER))
       (✓₀ (zmq-bind frontend frontend-channel))
       (✓₀ (zmq-bind backend backend-channel))
       (zmq_proxy frontend backend NULL))))

    (define broker/poll
     (zmq-component pid () (frontend-channel backend-channel)
      (zmq-socket ((frontend ZMQ_ROUTER)
                   (backend ZMQ_DEALER))
       (✓₀ (zmq-bind frontend frontend-channel))
       (✓₀ (zmq-bind backend backend-channel))
       (let ((↔ (lambda (recv-socket send-socket finished) 
                 (let ((more (void)))
                  (zmq-message (message)
                   (zmq_msg_recv message recv-socket 0)
                   (set! more (zmq_msg_more message))
                   (zmq_msg_send message send-socket 
                    (if (positive? more) ZMQ_SNDMORE 0)))
                  (unless (positive? more) (finished))))))
        (forever
         (zmq-poll
          ((frontend ZMQ_POLLIN) (forever (break) (↔ frontend backend break)))
          ((backend ZMQ_POLLIN) (forever (break) (↔ backend frontend break)))))))))

)



