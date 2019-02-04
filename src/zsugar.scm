
(module zsugar *

 (import scheme
  (chicken base)
  (except (chicken foreign) location)
  (chicken syntax)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zhelpers)

 (define-syntax zmq-context
  (syntax-rules ()
   ((zmq-context (ctx ...) body ...) (let ((ctx (zmq_ctx_new)) ...)
                                      body ...
                                      (zmq_ctx_destroy ctx) ...))))

 (define-syntax zmq-socket
  (syntax-rules (ZMQ_SUB)

   ; implicit context when only one of it is required.
   ((zmq-socket (socket ...) body ...) 
    (zmq-context (ctx) 
     (zmq-socket ctx (socket ...) body ...)))

   ; base case, no more bindings to handle, just put the `body` in.
   ((zmq-socket ctx () body ...) (begin body ...))

   ; special case for `SUB` socket, it enforces to `SUBSCRIBE` an `id`. 
   ((zmq-socket ctx ((socket (ZMQ_SUB id)) other ...) body ...)
    (zmq-socket ctx ((socket ZMQ_SUB) other ...)
     (begin
      (✓₀ (zmq_setsockopt socket ZMQ_SUBSCRIBE (location id) (string-length id)))
      body ...)))

   ; catch all case for any sockets; btw, it recurs to handle each socket type.
   ((zmq-socket ctx ((socket type) other ...) body ...) 
    (let ((socket (zmq_socket ctx type)))
     (zmq-socket ctx (other ...)
      body ...
      (zmq_close socket))))))

    (define-syntax zmq-poll
     (syntax-rules (→)
      ((zmq-poll ((item → body) ...))
       (let ((items (list item ...))) 
        (zmq_poll (location items) (length items) -1)
        (when (bitwise-and (zmq_pollitem_t-revents item) ZMQ_POLLIN)
         body) ...))
      ((zmq-poll ((socket body ...) ...)) 
       (zmq-poll 
        (((make-zmq_pollitem_t socket 0 ZMQ_POLLIN 0) → (begin body ...)) ...)))))

 (define-syntax ✓₀
  (syntax-rules ()
   ((✓₀ sexp) (assert (zero? sexp)))))

 (define-syntax forever
  (syntax-rules ()
   ((forever (break) body ...) (call/cc (lambda (break)
                                         (let loop ()
                                          body ...
                                          (loop)))))
   ((forever body ...) (forever (_) body ...))))


 )
