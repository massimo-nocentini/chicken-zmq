
(module zmq *

 (import scheme (chicken base) (chicken foreign))

 (foreign-declare "#include <zmq.h>")

 (define zmq_ctx_new (foreign-lambda c-pointer "zmq_ctx_new")) 
 (define zmq_ctx_get (foreign-lambda int "zmq_ctx_get" c-pointer int)) 
 (define zmq_errno (foreign-lambda int "zmq_errno")) 
 (define zmq_socket (foreign-lambda c-pointer "zmq_socket" c-pointer int)) 
 (define zmq_bind (foreign-lambda int "zmq_bind" c-pointer (const c-string)))
 (define zmq_recv (foreign-lambda int "zmq_recv" c-pointer c-pointer size_t int))
 (define zmq_send (foreign-lambda int "zmq_send" c-pointer (const c-pointer) size_t int))
 (define zmq_close (foreign-lambda int "zmq_close" c-pointer))
 (define zmq_ctx_destroy (foreign-lambda int "zmq_ctx_destroy" c-pointer))
 (define zmq_connect (foreign-lambda int "zmq_connect" c-pointer (const c-string)))

 (define-foreign-variable ZMQ_REP_inner int "ZMQ_REP") 
 (define-foreign-variable ZMQ_REQ_inner int "ZMQ_REQ") 

 (define ZMQ_REP ZMQ_REP_inner) 
 (define ZMQ_REQ ZMQ_REQ_inner) 

)
