
(module zhelpers *

 (import scheme (chicken base) (chicken foreign))
 (import zmq)

 (foreign-declare "#include \"zhelpers.h\"")

 (define s_send (foreign-lambda int "s_send" c-pointer c-string))
 (define s_recv (foreign-lambda c-string "s_recv" c-pointer))
 (define s_sleep (foreign-lambda void "s_sleep" int))
 (define s_clock (foreign-lambda integer64 "s_clock"))


    ; my own helpers.
    (define zmq-msg-recv
     (lambda (message socket kont)
      (let ((msg (location message)))
       (zmq_msg_recv msg socket 0)
       (kont (zmq_msg_more msg)))))

    (define zmq-msg-send
     (lambda (message socket more)
      (let ((msg (location message)))
       (zmq_msg_send msg socket (if more ZMQ_SNDMORE 0)))))

)
