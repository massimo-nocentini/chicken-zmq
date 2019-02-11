
(module zmq *

 (import scheme (chicken format) (chicken base) (chicken foreign) )
 (import-for-syntax (chicken format))

 (foreign-declare "#include <zmq.h>")

 (define zmq_ctx_new (foreign-lambda c-pointer "zmq_ctx_new"))
 (define zmq_ctx_get (foreign-lambda int "zmq_ctx_get" c-pointer int))
 (define zmq_errno (foreign-lambda int "zmq_errno"))
 (define zmq_strerror (foreign-lambda (const c-string) "zmq_strerror" int))
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

 ;(define zmq_poller_new (foreign-lambda c-pointer "zmq_poller_new"))
 ;(define zmq_poller_destroy (foreign-lambda int "zmq_poller_destroy" (c-pointer c-pointer)))
 ;(define zmq_poller_add (foreign-lambda int "zmq_poller_add" c-pointer c-pointer c-pointer short))
 ;(define zmq_poller_modify (foreign-lambda int "zmq_poller_modify" c-pointer c-pointer c-pointer short))
 ;(define zmq_poller_remove (foreign-lambda int "zmq_poller_remove" c-pointer c-pointer short))
 ;(define zmq_poller_wait (foreign-lambda int "zmq_poller_wait" c-pointer (c-pointer (struct "zmq_poller_event_t")) long))
 ;(define zmq_poller_wait_all (foreign-lambda int "zmq_poller_wait_all" c-pointer (c-pointer (struct "zmq_poller_event_t")) int long))

 ; Socket types.
 (define-foreign-variable ZMQ_REP_inner int "ZMQ_REP")
 (define-foreign-variable ZMQ_REQ_inner int "ZMQ_REQ")
 (define-foreign-variable ZMQ_PUB_inner int "ZMQ_PUB")
 (define-foreign-variable ZMQ_SUB_inner int "ZMQ_SUB")
 (define-foreign-variable ZMQ_PUSH_inner int "ZMQ_PUSH")
 (define-foreign-variable ZMQ_PULL_inner int "ZMQ_PULL")
 (define-foreign-variable ZMQ_ROUTER_inner int "ZMQ_ROUTER")
 (define-foreign-variable ZMQ_DEALER_inner int "ZMQ_DEALER")
 (define-foreign-variable ZMQ_XPUB_inner int "ZMQ_XPUB")
 (define-foreign-variable ZMQ_XSUB_inner int "ZMQ_XSUB")

 (define ZMQ_REP ZMQ_REP_inner)
 (define ZMQ_REQ ZMQ_REQ_inner)
 (define ZMQ_PUB ZMQ_PUB_inner)
 (define ZMQ_SUB ZMQ_SUB_inner)
 (define ZMQ_PUSH ZMQ_PUSH_inner)
 (define ZMQ_PULL ZMQ_PULL_inner)
 (define ZMQ_ROUTER ZMQ_ROUTER_inner)
 (define ZMQ_DEALER ZMQ_DEALER_inner)
 (define ZMQ_XPUB ZMQ_XPUB_inner)
 (define ZMQ_XSUB ZMQ_XSUB_inner)

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

    (define make-zmq_pollitem_t
     (foreign-lambda* (c-pointer (struct "zmq_pollitem_t"))
      ((int nbr))
      "zmq_pollitem_t items [nbr]; 
      C_return(items);"))

    (define set-zmq_pollitem_t!
     (foreign-lambda* void
      (((c-pointer (struct "zmq_pollitem_t")) items)
       (int i)
       (c-pointer socket)
       (short events))
      "zmq_pollitem_t item = { .socket = socket, .fd = 0, .events = events, .revents = 0};
      items[i] = item;"))

    (define zmq_pollitem_t-revents
     (foreign-lambda* short
      (((c-pointer (struct "zmq_pollitem_t")) items)
       (int i))
      "C_return(items[i].revents);"))

    (define make-zmq_msg_t
     (foreign-lambda* (c-pointer (struct "zmq_msg_t"))
      ()
      "zmq_msg_t msg; 
      C_return(&msg);"))

)
