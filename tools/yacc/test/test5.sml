(* test translation to NF *)


(* replaced 'lexer' with 'lexxer' because of problems with hose *)


structure c = 
struct
   
    rule a = n as skip => (nleft)

    parser parse = a
 
    val lexxer = fn () => (NONE,0,0) 
end
