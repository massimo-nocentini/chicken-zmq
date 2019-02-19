
(module zsugar *

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken syntax)
  (chicken bitwise)
  (chicken process)
  (chicken module)
  (chicken port)
  (chicken repl)
  (chicken eval)
  (chicken io)
  (chicken condition)
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
     (lambda ()
      (let ((PIDs (list))
            (outputs (list)))
       (let ((PIDs-getter (lambda (f) (map f PIDs)))
             (outputs-getter (lambda () (list-copy outputs)))
             (fork (lambda (id recv)
                    (let* ((filename (string-append id ".out"))
                           (pid (process-fork
                                 (lambda ()
                                  (with-output-to-file filename 
                                   (lambda () (recv current-process-id)) #:text)))))
                     (push! outputs filename)
                     (push! PIDs pid)
                     (values pid filename)))))
        (values fork PIDs-getter outputs-getter)))))

    (define write-bash-script 
     (lambda (filename cmd options args) ; `options` and `args` have to have type [string] both of them.
      (with-output-to-file filename
       (lambda ()
        (format #t "~a ~{~a ~}~%~!" cmd (append options args))))))
    ; the above definition arises from
    ; (write-bash-script "kill-all.sh" "kill" (list "-9") (PIDs-getter number->string))

    (define periodically
     (lambda (period recv)
      (forever
       (recv (s_clock)) 
       (s_sleep period))))

    (define replace-process-with
     (lambda (cmd options args) ; `options` and `args` have to have type [string] both of them.
      (process-execute cmd (append options args))))
    ; the above definitions has its origin in the expression that starts a `tail` watcher
    ; (replace-process-with "tail" (list "-f") (PIDs-getter number->string))

    (define exe-cmd&>output
     (lambda (sh-cmd)
      (let-values (((in-port out-port pid₀) (process sh-cmd)))
       (read-string #f in-port))))

    (define tail-watcher
     (lambda (period args-getter)
      (lambda ()
       (let ((handler (lambda (e) 
                       (print "tail-watcher gracefully interrupted")))
             (thunk (lambda ()
                     (periodically period 
                      (lambda (time) 
                       (print (exe-cmd&>output (format #f "tail ~{~a ~}~%~!" (args-getter)))))))))
        (with-exception-handler handler thunk)))))

    (define start-repl 
     (lambda (module-name)
      (repl (lambda (expr) 
             (eval expr (module-environment module-name))))))

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
