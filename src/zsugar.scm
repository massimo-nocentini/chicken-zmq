
(module zsugar *

 (import scheme
  (chicken base)
  (except (chicken foreign) location)
  (chicken syntax)
  (chicken bitwise)
  (chicken process-context)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq zhelpers)

 (define-syntax zmq-context
  (syntax-rules ()
   ((zmq-context (ctx ...) body ...)
    (let ((ctx (zmq_ctx_new)) ...)
     (✓ ctx) ...
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
     (✓ socket)
     (zmq-socket ctx (other ...)
      body ...
      (zmq_close socket))))))

    (define-syntax zmq-poll
     (syntax-rules (↑ →)
      ((zmq-poll ↑ _₀ _₁ ()) (void))
      ((zmq-poll ↑ n items ((socket events) other ...))
       (begin
        (set-zmq_pollitem_t! items n socket events)
        (zmq-poll ↑ (add1 n) items (other ...))))

      ((zmq-poll → _₀ _₁ ()) (void))
      ((zmq-poll → n items ((events body) other ...))
       (begin
        (when (positive? (bitwise-and (zmq_pollitem_t-revents items n) events))
         body)
        (zmq-poll → (add1 n) items (other ...))))

      ((zmq-poll ((socket events) body) ...)
       (let* ((n (vector-length (vector socket ...)))
              (items (make-zmq_pollitem_t n)))
        (assert (pointer? items))
        (zmq-poll ↑ 0 items ((socket events) ...))
        (✗₋₁ (zmq_poll items n -1))
        (zmq-poll → 0 items ((events body) ...))))))

    (define-syntax zmq-message
     (syntax-rules ()
      ((zmq-message (msg ...) body ...)
       (let ((msg (make-zmq_msg_t)) ...)
        (zmq_msg_init msg) ...
        body ...
        (zmq_msg_close msg) ...))))

    (define-syntax locations
     (syntax-rules ()
      ((locations (((obj type) &obj) ...) body ...)
       (let-location ((obj type) ...)
        (let ((&obj (location obj)) ...)
         body ...)))))

 (define-syntax ✓₀
  (syntax-rules ()
   ((✓₀ sexp) (assert (zero? sexp)))))

 (define-syntax ✓
  (syntax-rules ()
   ((✓ sexp) (assert (not (equal? sexp NULL))))))

    (define-syntax ✗₋₁
     (syntax-rules ()
      ((✗₋₁ sexp) (assert (not (negative? sexp))))
      ((✗₋₁ sexp recv)
       (let ((rc sexp))
        (cond
         ((equal? rc -1) (recv (zmq_errno)))
         (else (✓₀ rc)))))))

 (define-syntax forever
  (syntax-rules ()
   ((forever (break) body ...) (call/cc (lambda (break)
                                         (let loop ()
                                          body ...
                                          (loop)))))
   ((forever body ...) (forever (_) body ...))))

 (define NULL (foreign-value "NULL" c-pointer))

 )
