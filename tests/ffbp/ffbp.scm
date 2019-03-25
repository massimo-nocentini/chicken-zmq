
(module ffbp *
 
 (import scheme (chicken base) (chicken format))
 (import srfi-1 srfi-13 srfi-133)
 (import matchable format)
 (import zsugar)

 (define times
  (lambda (n doer)
   (for-each doer (iota n))))

 (define vector-filled?
  (lambda (α)
   (let ((A (lambda (x) (not (eq? x (void))))))
    (vector-every A α))))

    (define ·
     (lambda (α β)
      (foldr + 0 (map * α β))))

    (define vector-serialize
     (lambda (α)
      (let ((β (vector-map number->string α)))
       (format #f "~{~a ~}" β))))

    (define vector-deserialize
     (o list->vector (lambda (lst) (map string->number lst)) string-tokenize))

 (define receive
  (lambda (socket n post)
   (let ((α (make-vector n (void)))
         (R (lambda (recv)
             (match-let (((_ k v ...) (string-tokenize (s_recv socket))))
              (recv (string->number k) (apply post v))))))
    (times n (lambda _ (R (lambda (k v) (vector-set! α k v)))))
    (assert (vector-filled? α))
    α)))

    (define input
     (zmq-component pid (i feature rhs-#) (<- ->) 
      (zmq-socket ((publisher ZMQ_PUB)
                   (subscriber ZMQ_SUB (ZMQ_SUBSCRIBE "")))
       (✓₀ (zmq-connect publisher ->))
       (✓₀ (zmq-connect subscriber <-))
       (let ((F (lambda (x) 
                 (let ((msg (format #f "* ~a ~a" i x))
                       (s_send publisher msg)
                       (let ((α (receive subscriber rhs-# vector-deserialize)))
                        (print α) ; `α` is a vector of vectors
                        α))))))
        (map F feature)))))

    (define hidden
     (zmq-component pid (i σ δσ lhs/# n/# rhs/#) (lhs/<- lhs/-> rhs/<- rhs/->) 
      (zmq-socket ((lhs/publisher ZMQ_PUB) (lhs/subscriber ZMQ_SUB (ZMQ_SUBSCRIBE ""))
                   (rhs/publisher ZMQ_PUB) (rhs/subscriber ZMQ_SUB (ZMQ_SUBSCRIBE "")))
       (✓₀ (zmq-connect lhs/publisher lhs/->))
       (✓₀ (zmq-connect lhs/subscriber lhs/<-))
       (✓₀ (zmq-connect rhs/publisher rhs/->))
       (✓₀ (zmq-connect rhs/subscriber rhs/<-))
       (let ((α (make-vector lhs/# (void))))
        (forever
         (let* ((X (receive lhs/subscriber lhs/# string->number))
                (v (· α X))
                (z (σ v)))
          (s_send rhs/publisher (number->string z))
          (let ((βs (receive rhs/subscriber rhs/# vector-deserialize))
                (Σ (vector-fold-right (lambda (α β) (vector-map + α β)) (make-vector n/# 0)))
                (w (* (δσ v) (· Σ (make-vector (vector-length Σ) 1))))
                (X₀ (vector-map (lambda (x) (* w x)) X)))
           (set! α (vector-map - α X₀))))
         (s_send lhs/publisher (vector-serialize α)))))))
                
    )
