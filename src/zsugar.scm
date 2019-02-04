
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
   ((zmq-socket ctx () body ...) (begin body ...))
   ((zmq-socket ctx ((socket (ZMQ_SUB sub)) other ...) body ...)
    (zmq-socket ctx ((socket ZMQ_SUB) other ...)
     (begin
      (✓₀ (zmq_setsockopt socket ZMQ_SUBSCRIBE 
           (location sub) (string-length sub)))
      body ...)))
   ((zmq-socket ctx ((socket type) other ...) body ...) 
    (let ((socket (zmq_socket ctx type)))
     (zmq-socket cts (other ...)
      body ...
      (zmq_close socket))))))

 (define-syntax ✓₀
  (syntax-rules ()
   ((✓₀ sexp) (assert (equal? 0 sexp)))))

 (define-syntax forever
  (syntax-rules ()
   ((forever (break) body ...) (call/cc (lambda (break)
                                         (let loop ()
                                          body ...
                                          (loop)))))
   ((forever body ...) (forever (_) body ...))))


 )
