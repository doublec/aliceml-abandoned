(* test translation to NF *)
structure c = 
struct
   
    rule a = n as skip => (nleft)

    parser parse = a
 
    val lexer = fn () => (NONE,0,0) 
end
