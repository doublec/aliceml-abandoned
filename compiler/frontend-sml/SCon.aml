(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *
 * Note:
 *   I would like to use WideChar and WideString, but SML/NJ does not
 *   support it.
 *)


structure SCon :> SCON =
  struct

    datatype SCon =
	  INT    of LargeInt.int
	| WORD   of LargeWord.word
	| STRING of String.string
	| CHAR   of Char.char
	| REAL   of string

    type t = SCon

    fun toString(INT i)    = LargeInt.toString i
      | toString(WORD w)   = "0wx" ^ LargeWord.toString w
      | toString(STRING s) = "\""  ^ String.toCString s ^ "\""
      | toString(CHAR c)   = "\"#" ^ Char.toCString c   ^ "\""
      | toString(REAL r)   = r

  end
