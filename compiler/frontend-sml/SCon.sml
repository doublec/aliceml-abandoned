(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *)


structure SCon :> SCON =
  struct

    datatype SCon =
	  INT    of int
	| WORD   of word
	| STRING of string
	| CHAR   of char
	| REAL   of string

    type t = SCon

    fun toString(INT i)    = Int.toString i
      | toString(WORD w)   = "0wx" ^ Word.toString w
      | toString(STRING s) = "\""  ^ String.toCString s ^ "\""
      | toString(CHAR c)   = "\"#" ^ Char.toCString c   ^ "\""
      | toString(REAL r)   = r

  end
