signature LEXER =
    sig

	exception Error

	val lexer :
	    (int vector * string vector * bool)
	    * ('a * int * string * int * int -> 'b)
	    * int vector 
	    ->
	    (unit -> char option) * bool ref
	    * char option list ref * char option list ref
	    * int ref * int ref * 'a
	    ->
	    (unit -> 'b)

	val fromString : string -> (unit -> char option)

	val fromStream : TextIO.instream -> (unit -> char option)

    end
