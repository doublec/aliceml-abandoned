(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *)


signature SCON =    
  sig 

    datatype SCon =
	  INTEGER of int
	| WORD    of word
	| STRING  of string
	| CHAR    of char
	| REAL    of real

    val fromInt:    int    -> SCon
    val fromWord:   word   -> SCon
    val fromString: string -> SCon
    val fromChar:   char   -> SCon
    val fromReal:   real   -> SCon

    val toString:   SCon   -> string

  end
