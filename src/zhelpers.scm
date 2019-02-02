
(module zhelpers *

 (import scheme (chicken base) (chicken foreign))

 (foreign-declare "#include \"zhelpers.h\"")

 (define s_send (foreign-lambda int "s_send" c-pointer c-string))
 (define s_recv (foreign-lambda c-string "s_recv" c-pointer))

)
