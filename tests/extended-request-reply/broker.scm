

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken memory)
  (chicken process-context))
 (import srfi-13)
 (import zmq zhelpers zsugar)

    (define (start-broker)
     (zmq-socket ((frontend ZMQ_ROUTER)
                  (backend ZMQ_DEALER))
      (✓₀ (zmq_bind frontend "tcp://*:5559"))
      (✓₀ (zmq_bind backend "tcp://*:5560"))
      (let ((↔ (lambda (recv-socket send-socket finished) 
                (let ((more (void)))
                 (zmq-message (message)
                  (zmq_msg_recv #$message recv-socket 0)
                  (set! more (zmq_msg_more #$message))
                  (zmq_msg_send #$message send-socket 
                   (if (positive? more) ZMQ_SNDMORE 0)))
                 (unless (positive? more) (finished))))))
       (forever
        (zmq-poll
         (frontend (forever (break)
                    (↔ frontend backend break)))
         (backend (forever (break)
                   (↔ backend frontend break))))))))

 (start-broker) ; entry point



