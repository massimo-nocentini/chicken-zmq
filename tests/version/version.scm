

 (import scheme
  (chicken base)
  (chicken foreign)
  (chicken process-context)
  (chicken format)
  (chicken memory))
 (import srfi-1 srfi-13)
 (import zmq)

    (define version
     (lambda ()
      (let-location ((major int) (minor int) (patch int))
       (zmq_version (location major) (location minor) (location patch))
       (print* `(Current ∅MQ version is ,(list major minor patch)) "\n"))))

    (version) ; entry point
