(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *
 * Note:
 *   I would like to use WideChar and WideString, but SML/NJ does not
 *   support it.
 *)


signature SCON =    
  sig 

    datatype SCon =
	  INT    of LargeInt.int
	| WORD   of LargeWord.word
	| STRING of String.string
	| CHAR   of Char.char
	| REAL   of string

    type t = SCon

    val toString: SCon -> string

  end
