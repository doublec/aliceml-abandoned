(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *)


structure SCon :> SCON =
  struct

    datatype SCon =
	  INT    of LargeInt.int
	| WORD   of LargeWord.word
	| STRING of WideString.string
	| CHAR   of WideChar.char
	| REAL   of LargeReal.real

    type t = SCon

    fun toString(INT i)    = LargeInt.toString i
      | toString(WORD w)   = "0wx" ^ LargeWord.toString w
      | toString(STRING s) = "\""  ^ WideString.toCString s ^ "\""
      | toString(CHAR c)   = "\"#" ^ WideChar.toCString c   ^ "\""
      | toString(REAL r)   = LargeReal.toString r

  end
