signature LEXER =
    sig

	exception Error

	val lexer :
	    (int vector * string vector * bool)
	    * ('a * int * string * int * int -> 'b)
	    * int vector 
	    -> string * int ref * int ref * 'a
	    -> (unit -> 'b)

    end
