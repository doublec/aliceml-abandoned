(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *)


signature SCON =    
  sig 

    datatype SCon =
	  INT    of int
	| WORD   of word
	| STRING of string
	| CHAR   of char
	| REAL   of string

    type t = SCon

    val toString: SCon -> string

  end
