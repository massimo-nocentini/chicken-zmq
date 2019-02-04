
(module zhelpers *

 (import scheme (chicken base) (chicken foreign))

 (foreign-declare "#include \"zhelpers.h\"")

 (define s_send (foreign-lambda int "s_send" c-pointer c-string))
 (define s_recv (foreign-lambda c-string "s_recv" c-pointer))
 (define s_sleep (foreign-lambda void "s_sleep" int))
 (define s_clock (foreign-lambda integer64 "s_clock"))

)
