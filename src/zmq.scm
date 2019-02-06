
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
 (define zmq_version (foreign-lambda void "zmq_version" (c-pointer int) (c-pointer int) (c-pointer int)))
 (define zmq_setsockopt (foreign-lambda int "zmq_setsockopt" c-pointer int (const c-pointer) size_t))
 (define zmq_poll (foreign-lambda int "zmq_poll" (c-pointer (struct "zmq_pollitem_t")) int long))
 (define zmq_msg_init (foreign-lambda int "zmq_msg_init" (c-pointer (struct "zmq_msg_t"))))
 (define zmq_msg_more (foreign-lambda int "zmq_msg_more" (const (c-pointer (struct "zmq_msg_t")))))
 (define zmq_msg_close (foreign-lambda int "zmq_msg_close" (c-pointer (struct "zmq_msg_t"))))
 (define zmq_msg_send (foreign-lambda int "zmq_msg_send" (c-pointer (struct "zmq_msg_t")) c-pointer int))
 (define zmq_msg_recv (foreign-lambda int "zmq_msg_recv" (c-pointer (struct "zmq_msg_t")) c-pointer int))
 (define zmq_proxy (foreign-lambda int "zmq_proxy" c-pointer c-pointer c-pointer))

 ; Socket types.
 (define-foreign-variable ZMQ_REP_inner int "ZMQ_REP") 
 (define-foreign-variable ZMQ_REQ_inner int "ZMQ_REQ") 
 (define-foreign-variable ZMQ_PUB_inner int "ZMQ_PUB") 
 (define-foreign-variable ZMQ_SUB_inner int "ZMQ_SUB") 
 (define-foreign-variable ZMQ_PUSH_inner int "ZMQ_PUSH") 
 (define-foreign-variable ZMQ_PULL_inner int "ZMQ_PULL") 
 (define-foreign-variable ZMQ_ROUTER_inner int "ZMQ_ROUTER") 
 (define-foreign-variable ZMQ_DEALER_inner int "ZMQ_DEALER") 

 (define ZMQ_REP ZMQ_REP_inner) 
 (define ZMQ_REQ ZMQ_REQ_inner) 
 (define ZMQ_PUB ZMQ_PUB_inner) 
 (define ZMQ_SUB ZMQ_SUB_inner) 
 (define ZMQ_PUSH ZMQ_PUSH_inner) 
 (define ZMQ_PULL ZMQ_PULL_inner) 
 (define ZMQ_ROUTER ZMQ_ROUTER_inner) 
 (define ZMQ_DEALER ZMQ_DEALER_inner) 

 (define-foreign-variable ZMQ_POLLIN_inner int "ZMQ_POLLIN") 
 (define ZMQ_POLLIN ZMQ_POLLIN_inner) 

 ; Send/recv options.                                                        */
 (define-foreign-variable ZMQ_DONTWAIT_inner int "ZMQ_DONTWAIT") 
 (define-foreign-variable ZMQ_SNDMORE_inner int "ZMQ_SNDMORE") 
 (define ZMQ_DONTWAIT ZMQ_DONTWAIT_inner) 
 (define ZMQ_SNDMORE ZMQ_SNDMORE_inner) 

 ; Socket options.
 (define-foreign-variable ZMQ_SUBSCRIBE_inner int "ZMQ_SUBSCRIBE") 
 (define ZMQ_SUBSCRIBE ZMQ_SUBSCRIBE_inner) 

 (define-record zmq_pollitem_t socket fd events revents)
 (define-record zmq_msg_t _)

 ; types.
 ;(define ty_zmq_msg_t (struct "zmq_msg_t")) 
)
