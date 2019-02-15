
(module zsugar *

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken syntax)
  (chicken bitwise)
  (chicken process)
  (chicken module)
  (chicken port)
  (chicken process-context)
  (chicken process-context posix) 
  (chicken file posix)
  (chicken memory))
 (import format) ; dedicated `format` module, CLISP style.
 (import srfi-1 srfi-13)
 (import zmq zhelpers)

 ; some utilities
 (define-syntax push!
  (syntax-rules ()
   ((push! lst a) (set! lst (cons a lst)))))

 ; necessary to avoid clients to import `(chicken foreign)` that causes
 ; compilation problems and, worse, at runtime.
 (define zmq-location 
  (lambda (obj) 
   (location obj)))

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
      (✓₀ (zmq_setsockopt socket ZMQ_SUBSCRIBE (zmq-location id) (string-length id)))
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


(define-record channel protocol address port)

    (define channel->string/connect
     (lambda (channel)
      (format #f "~a://~a:~a"
       (channel-protocol channel)
       (channel-address channel)
       (channel-port channel))))

    (define channel->string/bind
     (lambda (channel)
      (format #f "~a://*:~a"
       (channel-protocol channel)
       (channel-port channel))))

    (define zmq-system
     (lambda (spec kill-scripter)
      (let* ((PIDs (list))
             (watching (list))
             (fork (lambda (id recv)
                    (let* ((filename (string-append id ".out"))
                           (pid (process-fork
                                 (lambda ()
                                  (let ((port (open-output-file filename)))
                                   (with-output-to-port port 
                                    (lambda () 
                                     (recv current-process-id)))
                                   (close-output-port port))))))
                     (push! watching filename)
                     (push! PIDs pid))))
             (writer (lambda (kill-script)
                      (let ((port (open-output-file kill-script)))
                       (format port "kill -9 ~{~a ~}~%~!" (map number->string PIDs))
                       (close-output-port port))))
             (watcher (lambda () (process-execute "tail" (cons "-f" watching)))))
       (spec fork) ; fork components.
       (kill-scripter writer) ; write bash script to kill them all, for rescue.
       (watcher) ; watch the system, finally.
    )))

    (define-syntax zmq-component
     (syntax-rules ()
      ((zmq-component pid (arg ...) (channel ...) body ...)
       (lambda (arg ...)
        (lambda (channel ...)
         (lambda (pid-getter)
          (let ((pid (pid-getter)))
           (assert (positive? pid))
           body ...)))))))


    (define zmq-bind
     (lambda (socket channel)
      (zmq_bind socket (channel->string/bind channel))))

    (define zmq-connect
     (lambda (socket channel)
      (zmq_connect socket (channel->string/connect channel))))

    (define zmq-recv
     (lambda (socket bytes)
      (let ((buffer (make-string bytes)))
       (zmq_recv socket (zmq-location buffer) bytes 0)
       buffer)))

    (define zmq-send
     (lambda (socket str)
      (zmq_send socket (zmq-location str) (string-length str) 0)))

 )
